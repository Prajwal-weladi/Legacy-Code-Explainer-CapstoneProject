from fastapi import APIRouter, UploadFile, File, HTTPException, status
from typing import Dict, Any
import time
import aiofiles
from pathlib import Path

from models.schemas import (
    HealthResponse, AnalyzeResponse, ErrorResponse,
    ParsedData, Explanation
)
from config import settings
from parsers.cobol_parser import CobolParser
from parsers.jcl_parser import JCLParser
from services.explanation_service import ExplanationService
from services.llm_service import OllamaService

router = APIRouter()
explanation_service = ExplanationService()
ollama_service = OllamaService()

@router.get("/health", response_model=HealthResponse)
async def health_check():
    """Health check endpoint"""
    ollama_status = await ollama_service.is_available()
    
    return HealthResponse(
        status="healthy",
        ollama_available=ollama_status,
        ollama_host=settings.ollama_host,
        ollama_model=settings.ollama_model
    )

@router.post("/analyze", response_model=AnalyzeResponse)
async def analyze_code(file: UploadFile = File(...)):
    """Complete analysis: upload + parse + explain"""
    
    start_time = time.time()
    
    # Validate file
    if not file.filename:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="No file provided"
        )
    
    # Check extension
    ext = file.filename.rsplit('.', 1)[-1].lower()
    if ext not in settings.allowed_extensions:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail=f"Invalid file type. Allowed: {settings.allowed_extensions}"
        )
    
    try:
        # Read file content
        content = await file.read()
        code_content = content.decode('utf-8', errors='ignore')
        
        # Auto-detect code type
        code_type = 'cobol' if ext in ['cbl', 'cob'] else 'jcl'
        
        # Parse code
        parser = CobolParser(code_content) if code_type == 'cobol' else JCLParser(code_content)
        parsed_data = parser.parse()
        
        # Generate explanation
        explanation = await explanation_service.generate_explanation(parsed_data)
        
        processing_time = time.time() - start_time
        
        return AnalyzeResponse(
            success=True,
            filename=file.filename,
            code_type=code_type,
            parsed_data=ParsedData(**parsed_data),
            explanation=Explanation(**explanation),
            processing_time=round(processing_time, 2)
        )
    
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=f"Analysis failed: {str(e)}"
        )

@router.post("/parse", response_model=Dict[str, Any])
async def parse_only(file: UploadFile = File(...)):
    """Parse code without explanation"""
    
    if not file.filename:
        raise HTTPException(
            status_code=status.HTTP_400_BAD_REQUEST,
            detail="No file provided"
        )
    
    try:
        content = await file.read()
        code_content = content.decode('utf-8', errors='ignore')
        
        ext = file.filename.rsplit('.', 1)[-1].lower()
        code_type = 'cobol' if ext in ['cbl', 'cob'] else 'jcl'
        
        parser = CobolParser(code_content) if code_type == 'cobol' else JCLParser(code_content)
        parsed_data = parser.parse()
        
        return {
            'success': True,
            'filename': file.filename,
            'code_type': code_type,
            'parsed_data': parsed_data
        }
    
    except Exception as e:
        raise HTTPException(
            status_code=status.HTTP_500_INTERNAL_SERVER_ERROR,
            detail=str(e)
        )
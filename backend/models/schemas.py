from pydantic import BaseModel, Field
from typing import List, Dict, Optional, Any, Literal
from datetime import datetime

# Request Models
class AnalyzeRequest(BaseModel):
    code_type: Literal["cobol", "jcl", "auto"] = "auto"

# Response Models
class HealthResponse(BaseModel):
    status: str
    ollama_available: bool
    ollama_host: str
    ollama_model: str
    timestamp: datetime = Field(default_factory=datetime.now)

class Statistics(BaseModel):
    total_lines: int
    non_empty_lines: int = 0
    variable_count: Optional[int] = None
    procedure_count: Optional[int] = None
    step_count: Optional[int] = None
    dataset_count: Optional[int] = None

class ParsedData(BaseModel):
    type: str
    metadata: Optional[Dict[str, Any]] = None
    statistics: Statistics
    variables: Optional[List[Dict[str, Any]]] = None
    procedures: Optional[List[Dict[str, Any]]] = None
    steps: Optional[List[Dict[str, Any]]] = None
    datasets: Optional[List[Dict[str, Any]]] = None

class Explanation(BaseModel):
    summary: str
    business_logic: str
    technical_details: str
    modernization_suggestions: List[str]
    complexity_assessment: Literal["LOW", "MEDIUM", "HIGH"]
    raw_explanation: str

class AnalyzeResponse(BaseModel):
    success: bool
    filename: str
    code_type: str
    parsed_data: ParsedData
    explanation: Explanation
    processing_time: float

class ErrorResponse(BaseModel):
    success: bool = False
    error: str
    detail: Optional[str] = None
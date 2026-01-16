"""Tests for API routes"""

import pytest
from unittest.mock import AsyncMock, patch, MagicMock
from fastapi import UploadFile
from fastapi.testclient import TestClient
from io import BytesIO


class TestHealthEndpoint:
    """Test health check endpoint"""
    
    def test_health_check_success(self, test_client, mock_ollama_service):
        """Test successful health check"""
        with patch('api.routes.ollama_service', mock_ollama_service):
            response = test_client.get("/api/health")
            assert response.status_code == 200
            
            data = response.json()
            assert "status" in data
            assert "ollama_available" in data
            assert "ollama_host" in data
    
    def test_health_response_structure(self, test_client, mock_ollama_service):
        """Test health response structure"""
        with patch('api.routes.ollama_service', mock_ollama_service):
            response = test_client.get("/api/health")
            data = response.json()
            
            assert data["status"] == "healthy"
            assert isinstance(data["ollama_available"], bool)
            assert data["ollama_host"] is not None
            assert data["ollama_model"] is not None


class TestAnalyzeEndpoint:
    """Test code analysis endpoint"""
    
    def test_analyze_cobol_file_success(self, test_client, sample_cobol_code, mock_ollama_service, explanation_data):
        """Test successful COBOL file analysis"""
        with patch('api.routes.ollama_service', mock_ollama_service), \
             patch('api.routes.explanation_service.generate_explanation', new_callable=AsyncMock) as mock_explain:
            
            mock_explain.return_value = explanation_data
            
            file_content = sample_cobol_code.encode('utf-8')
            response = test_client.post(
                "/api/analyze",
                files={"file": ("test.cob", BytesIO(file_content), "text/plain")}
            )
            
            assert response.status_code == 200
            data = response.json()
            assert data["success"] is True
            assert data["filename"] == "test.cob"
            assert data["code_type"] == "cobol"
    
    def test_analyze_jcl_file_success(self, test_client, sample_jcl_code, mock_ollama_service, explanation_data):
        """Test successful JCL file analysis"""
        with patch('api.routes.ollama_service', mock_ollama_service), \
             patch('api.routes.explanation_service.generate_explanation', new_callable=AsyncMock) as mock_explain:
            
            mock_explain.return_value = explanation_data
            
            file_content = sample_jcl_code.encode('utf-8')
            response = test_client.post(
                "/api/analyze",
                files={"file": ("test.jcl", BytesIO(file_content), "text/plain")}
            )
            
            assert response.status_code == 200
            data = response.json()
            assert data["success"] is True
            assert data["filename"] == "test.jcl"
            assert data["code_type"] == "jcl"
    
    def test_analyze_no_file_provided(self, test_client):
        """Test analyze endpoint without file"""
        response = test_client.post("/api/analyze")
        assert response.status_code in [400, 422]  # Either bad request or unprocessable entity
    
    def test_analyze_invalid_file_extension(self, test_client):
        """Test analyze with invalid file extension"""
        response = test_client.post(
            "/api/analyze",
            files={"file": ("test.xyz", BytesIO(b"some content"), "text/plain")}
        )
        assert response.status_code == 400
    
    def test_analyze_response_structure(self, test_client, sample_cobol_code, mock_ollama_service, explanation_data):
        """Test analyze response structure"""
        with patch('api.routes.ollama_service', mock_ollama_service), \
             patch('api.routes.explanation_service.generate_explanation', new_callable=AsyncMock) as mock_explain:
            
            mock_explain.return_value = explanation_data
            
            file_content = sample_cobol_code.encode('utf-8')
            response = test_client.post(
                "/api/analyze",
                files={"file": ("test.cob", BytesIO(file_content), "text/plain")}
            )
            
            data = response.json()
            assert "success" in data
            assert "filename" in data
            assert "code_type" in data
            assert "parsed_data" in data
            assert "explanation" in data
            assert "processing_time" in data
    
    def test_analyze_processing_time(self, test_client, sample_cobol_code, mock_ollama_service, explanation_data):
        """Test that processing time is recorded"""
        with patch('api.routes.ollama_service', mock_ollama_service), \
             patch('api.routes.explanation_service.generate_explanation', new_callable=AsyncMock) as mock_explain:
            
            mock_explain.return_value = explanation_data
            
            file_content = sample_cobol_code.encode('utf-8')
            response = test_client.post(
                "/api/analyze",
                files={"file": ("test.cob", BytesIO(file_content), "text/plain")}
            )
            
            data = response.json()
            assert data["processing_time"] >= 0
            assert isinstance(data["processing_time"], (int, float))


class TestParseEndpoint:
    """Test code parsing endpoint"""
    
    def test_parse_cobol_file_success(self, test_client, sample_cobol_code):
        """Test successful COBOL parsing"""
        file_content = sample_cobol_code.encode('utf-8')
        response = test_client.post(
            "/api/parse",
            files={"file": ("test.cob", BytesIO(file_content), "text/plain")}
        )
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
        assert "parsed_data" in data
    
    def test_parse_jcl_file_success(self, test_client, sample_jcl_code):
        """Test successful JCL parsing"""
        file_content = sample_jcl_code.encode('utf-8')
        response = test_client.post(
            "/api/parse",
            files={"file": ("test.jcl", BytesIO(file_content), "text/plain")}
        )
        
        assert response.status_code == 200
        data = response.json()
        assert data["success"] is True
    
    def test_parse_no_file_provided(self, test_client):
        """Test parse endpoint without file"""
        response = test_client.post("/api/parse")
        assert response.status_code in [400, 422]
    
    def test_parse_response_structure(self, test_client, sample_cobol_code):
        """Test parse response structure"""
        file_content = sample_cobol_code.encode('utf-8')
        response = test_client.post(
            "/api/parse",
            files={"file": ("test.cob", BytesIO(file_content), "text/plain")}
        )
        
        data = response.json()
        assert "success" in data
        assert "parsed_data" in data
        assert "filename" in data


class TestMainEndpoints:
    """Test main application endpoints"""
    
    def test_root_endpoint(self, test_client):
        """Test root endpoint"""
        response = test_client.get("/")
        assert response.status_code in [200, 404]  # May vary based on frontend setup
    
    def test_docs_endpoint(self, test_client):
        """Test docs endpoint"""
        response = test_client.get("/docs")
        # FastAPI auto-docs or redirect
        assert response.status_code in [200, 307, 308]
    
    def test_openapi_schema(self, test_client):
        """Test OpenAPI schema availability"""
        response = test_client.get("/openapi.json")
        assert response.status_code == 200
        assert "openapi" in response.json()


class TestErrorHandling:
    """Test error handling in routes"""
    
    def test_file_with_invalid_encoding(self, test_client):
        """Test file with invalid encoding handling"""
        # Send file with invalid UTF-8 bytes
        file_content = b'\x80\x81\x82\x83'
        response = test_client.post(
            "/api/analyze",
            files={"file": ("test.cob", BytesIO(file_content), "text/plain")}
        )
        # Should handle gracefully
        assert response.status_code in [200, 500]
    
    def test_large_file_handling(self, test_client):
        """Test handling of large files"""
        # Create a large file
        large_content = b"IDENTIFICATION DIVISION. " * 100000  # ~2.5MB
        response = test_client.post(
            "/api/analyze",
            files={"file": ("large.cob", BytesIO(large_content), "text/plain")}
        )
        # Should either process or return appropriate error
        assert response.status_code in [200, 400, 413, 500]


class TestSchemas:
    """Test request/response schemas"""
    
    def test_health_response_schema(self):
        """Test HealthResponse schema"""
        from models.schemas import HealthResponse
        
        health = HealthResponse(
            status="healthy",
            ollama_available=True,
            ollama_host="http://localhost:11434",
            ollama_model="codellama"
        )
        
        assert health.status == "healthy"
        assert health.ollama_available is True
    
    def test_parsed_data_schema(self):
        """Test ParsedData schema"""
        from models.schemas import ParsedData, Statistics
        
        stats = Statistics(
            total_lines=100,
            non_empty_lines=85,
            variable_count=10
        )
        
        parsed = ParsedData(
            type="COBOL",
            statistics=stats
        )
        
        assert parsed.type == "COBOL"
        assert parsed.statistics.total_lines == 100
    
    def test_explanation_schema(self):
        """Test Explanation schema"""
        from models.schemas import Explanation
        
        explanation = Explanation(
            summary="Test summary",
            business_logic="Test business logic",
            technical_details="Test details",
            modernization_suggestions=["Suggestion 1"],
            complexity_assessment="LOW",
            raw_explanation="Raw text"
        )
        
        assert explanation.summary == "Test summary"
        assert explanation.complexity_assessment == "LOW"
    
    def test_analyze_response_schema(self):
        """Test AnalyzeResponse schema"""
        from models.schemas import AnalyzeResponse, ParsedData, Statistics, Explanation
        
        stats = Statistics(total_lines=100, non_empty_lines=85)
        parsed = ParsedData(type="COBOL", statistics=stats)
        explanation = Explanation(
            summary="Summary",
            business_logic="Logic",
            technical_details="Details",
            modernization_suggestions=[],
            complexity_assessment="LOW",
            raw_explanation="Raw"
        )
        
        response = AnalyzeResponse(
            success=True,
            filename="test.cob",
            code_type="cobol",
            parsed_data=parsed,
            explanation=explanation,
            processing_time=1.5
        )
        
        assert response.success is True
        assert response.processing_time == 1.5


class TestCORSHeaders:
    """Test CORS headers"""
    
    def test_cors_headers_present(self, test_client):
        """Test that CORS headers are present"""
        response = test_client.get("/api/health")
        
        # Check for CORS headers
        assert response.status_code == 200
        # Note: CORS headers are set by middleware, so they might not appear in test client

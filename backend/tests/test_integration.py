"""Integration tests for the full application"""

import pytest
from unittest.mock import AsyncMock, patch
from io import BytesIO


class TestEndToEndAnalysis:
    """End-to-end analysis tests"""
    
    def test_cobol_analysis_pipeline(self, test_client, sample_cobol_code, explanation_data):
        """Test complete COBOL analysis pipeline"""
        with patch('api.routes.ollama_service.is_available', new_callable=AsyncMock, return_value=True), \
             patch('api.routes.explanation_service.generate_explanation', new_callable=AsyncMock) as mock_explain:
            
            mock_explain.return_value = explanation_data
            
            # Upload and analyze COBOL file
            file_content = sample_cobol_code.encode('utf-8')
            response = test_client.post(
                "/api/analyze",
                files={"file": ("sample.cob", BytesIO(file_content), "text/plain")}
            )
            
            assert response.status_code == 200
            data = response.json()
            assert data["success"] is True
            assert data["code_type"] == "cobol"
            assert data["filename"] == "sample.cob"
            assert "parsed_data" in data
            assert "explanation" in data
    
    def test_jcl_analysis_pipeline(self, test_client, sample_jcl_code, explanation_data):
        """Test complete JCL analysis pipeline"""
        with patch('api.routes.ollama_service.is_available', new_callable=AsyncMock, return_value=True), \
             patch('api.routes.explanation_service.generate_explanation', new_callable=AsyncMock) as mock_explain:
            
            mock_explain.return_value = explanation_data
            
            # Upload and analyze JCL file
            file_content = sample_jcl_code.encode('utf-8')
            response = test_client.post(
                "/api/analyze",
                files={"file": ("sample.jcl", BytesIO(file_content), "text/plain")}
            )
            
            assert response.status_code == 200
            data = response.json()
            assert data["success"] is True
            assert data["code_type"] == "jcl"
    
    def test_parse_then_analyze_flow(self, test_client, sample_cobol_code, explanation_data):
        """Test parse-first then analyze flow"""
        with patch('api.routes.explanation_service.generate_explanation', new_callable=AsyncMock) as mock_explain:
            
            mock_explain.return_value = explanation_data
            
            file_content = sample_cobol_code.encode('utf-8')
            
            # First parse
            parse_response = test_client.post(
                "/api/parse",
                files={"file": ("test.cob", BytesIO(file_content), "text/plain")}
            )
            assert parse_response.status_code == 200
            
            # Then analyze
            analyze_response = test_client.post(
                "/api/analyze",
                files={"file": ("test.cob", BytesIO(file_content), "text/plain")}
            )
            assert analyze_response.status_code == 200


class TestServiceIntegration:
    """Integration tests for services"""
    
    @pytest.mark.asyncio
    async def test_parser_to_explanation_cobol(self, sample_cobol_code, mock_ollama_service):
        """Test data flow from parser to explanation service"""
        from parsers.cobol_parser import CobolParser
        
        with patch('services.explanation_service.OllamaService', return_value=mock_ollama_service):
            # Parse
            parser = CobolParser(sample_cobol_code)
            parsed = parser.parse()
            
            # Generate explanation
            from services.explanation_service import ExplanationService
            service = ExplanationService()
            explanation = await service.generate_explanation(parsed)
            
            # Verify flow
            assert parsed['type'] == 'COBOL'
            assert explanation['summary'] is not None
    
    @pytest.mark.asyncio
    async def test_parser_to_explanation_jcl(self, sample_jcl_code, mock_ollama_service):
        """Test data flow from parser to explanation service for JCL"""
        from parsers.jcl_parser import JCLParser
        
        with patch('services.explanation_service.OllamaService', return_value=mock_ollama_service):
            # Parse
            parser = JCLParser(sample_jcl_code)
            parsed = parser.parse()
            
            # Generate explanation
            from services.explanation_service import ExplanationService
            service = ExplanationService()
            explanation = await service.generate_explanation(parsed)
            
            # Verify flow
            assert parsed['type'] == 'JCL'
            assert explanation['summary'] is not None


class TestHealthcheckIntegration:
    """Integration tests for health checks"""
    
    def test_health_check_with_unavailable_ollama(self, test_client, mock_ollama_service):
        """Test health check when Ollama is unavailable"""
        mock_ollama_service.is_available = AsyncMock(return_value=False)
        
        with patch('api.routes.ollama_service', mock_ollama_service):
            response = test_client.get("/api/health")
            assert response.status_code == 200
            data = response.json()
            assert data["ollama_available"] is False
    
    def test_health_check_with_available_ollama(self, test_client, mock_ollama_service):
        """Test health check when Ollama is available"""
        mock_ollama_service.is_available = AsyncMock(return_value=True)
        
        with patch('api.routes.ollama_service', mock_ollama_service):
            response = test_client.get("/api/health")
            assert response.status_code == 200
            data = response.json()
            assert data["ollama_available"] is True


class TestErrorRecovery:
    """Test error recovery and handling"""
    
    def test_recovery_from_parsing_error(self, test_client):
        """Test graceful recovery from parsing errors"""
        # Send malformed COBOL that might cause parsing issues
        malformed_cobol = "IDENTIFICATION DIVISION. PROGRAM-ID.  \n" + "X" * 10000
        
        file_content = malformed_cobol.encode('utf-8')
        response = test_client.post(
            "/api/parse",
            files={"file": ("malformed.cob", BytesIO(file_content), "text/plain")}
        )
        
        assert response.status_code in [200, 500]  # Should not crash
    
    def test_recovery_from_llm_error(self, test_client, sample_cobol_code):
        """Test graceful recovery from LLM errors"""
        with patch('api.routes.explanation_service.generate_explanation', new_callable=AsyncMock) as mock_explain:
            mock_explain.side_effect = Exception("LLM service error")
            
            file_content = sample_cobol_code.encode('utf-8')
            response = test_client.post(
                "/api/analyze",
                files={"file": ("test.cob", BytesIO(file_content), "text/plain")}
            )
            
            # Should handle error gracefully
            assert response.status_code in [400, 500]


class TestFileTypeDetection:
    """Test automatic file type detection"""
    
    def test_detect_cobol_from_cbl_extension(self, test_client, sample_cobol_code):
        """Test COBOL detection from .cbl extension"""
        file_content = sample_cobol_code.encode('utf-8')
        response = test_client.post(
            "/api/analyze",
            files={"file": ("test.cbl", BytesIO(file_content), "text/plain")}
        )
        
        if response.status_code == 200:
            data = response.json()
            assert data["code_type"] == "cobol"
    
    def test_detect_cobol_from_cob_extension(self, test_client, sample_cobol_code):
        """Test COBOL detection from .cob extension"""
        file_content = sample_cobol_code.encode('utf-8')
        response = test_client.post(
            "/api/analyze",
            files={"file": ("test.cob", BytesIO(file_content), "text/plain")}
        )
        
        if response.status_code == 200:
            data = response.json()
            assert data["code_type"] == "cobol"
    
    def test_detect_jcl_from_jcl_extension(self, test_client, sample_jcl_code):
        """Test JCL detection from .jcl extension"""
        file_content = sample_jcl_code.encode('utf-8')
        response = test_client.post(
            "/api/analyze",
            files={"file": ("test.jcl", BytesIO(file_content), "text/plain")}
        )
        
        if response.status_code == 200:
            data = response.json()
            assert data["code_type"] == "jcl"


class TestDataConsistency:
    """Test data consistency across modules"""
    
    def test_parsed_data_consistency(self, sample_cobol_code):
        """Test that parsed data is consistent"""
        from parsers.cobol_parser import CobolParser
        
        parser1 = CobolParser(sample_cobol_code)
        result1 = parser1.parse()
        
        parser2 = CobolParser(sample_cobol_code)
        result2 = parser2.parse()
        
        # Results should be identical for same input
        assert result1['type'] == result2['type']
        assert result1['statistics']['total_lines'] == result2['statistics']['total_lines']
    
    def test_multiple_analysis_calls_consistency(self, test_client, sample_cobol_code, explanation_data):
        """Test consistency across multiple analysis calls"""
        with patch('api.routes.explanation_service.generate_explanation', new_callable=AsyncMock) as mock_explain:
            mock_explain.return_value = explanation_data
            
            file_content = sample_cobol_code.encode('utf-8')
            
            response1 = test_client.post(
                "/api/analyze",
                files={"file": ("test.cob", BytesIO(file_content), "text/plain")}
            )
            
            response2 = test_client.post(
                "/api/analyze",
                files={"file": ("test.cob", BytesIO(file_content), "text/plain")}
            )
            
            data1 = response1.json()
            data2 = response2.json()
            
            # Results should be consistent
            assert data1["filename"] == data2["filename"]
            assert data1["code_type"] == data2["code_type"]


class TestConcurrency:
    """Test concurrent operations"""
    
    def test_multiple_file_uploads(self, test_client, sample_cobol_code, explanation_data):
        """Test handling multiple concurrent-like uploads"""
        with patch('api.routes.explanation_service.generate_explanation', new_callable=AsyncMock) as mock_explain:
            mock_explain.return_value = explanation_data
            
            file_content = sample_cobol_code.encode('utf-8')
            
            # Simulate multiple uploads
            responses = []
            for i in range(3):
                response = test_client.post(
                    "/api/analyze",
                    files={"file": (f"test{i}.cob", BytesIO(file_content), "text/plain")}
                )
                responses.append(response)
            
            # All should succeed
            for response in responses:
                assert response.status_code == 200

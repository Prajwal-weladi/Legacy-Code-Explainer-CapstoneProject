"""Tests for service modules"""

import pytest
from unittest.mock import AsyncMock, patch, MagicMock
from services.llm_service import OllamaService
from services.explanation_service import ExplanationService


class TestOllamaService:
    """Test Ollama Service"""
    
    def test_ollama_service_initialization(self, mock_settings):
        """Test OllamaService initialization"""
        with patch('services.llm_service.settings', mock_settings):
            service = OllamaService()
            assert service.host == mock_settings.ollama_host
            assert service.model == mock_settings.ollama_model
            assert service.timeout == mock_settings.ollama_timeout
    
    @pytest.mark.asyncio
    async def test_ollama_is_available_success(self, mock_ollama_service):
        """Test successful availability check"""
        result = await mock_ollama_service.is_available()
        assert result is True
        mock_ollama_service.is_available.assert_called_once()
    
    @pytest.mark.asyncio
    async def test_ollama_is_available_failure(self):
        """Test failed availability check"""
        service = OllamaService()
        with patch('httpx.AsyncClient') as mock_client:
            mock_client.return_value.__aenter__.return_value.get.side_effect = Exception("Connection refused")
            result = await service.is_available()
            assert result is False
    
    @pytest.mark.asyncio
    async def test_ollama_generate_success(self, mock_ollama_service):
        """Test successful text generation"""
        result = await mock_ollama_service.generate("Test prompt")
        assert result == "This is a test explanation."
        mock_ollama_service.generate.assert_called_once_with("Test prompt")
    
    @pytest.mark.asyncio
    async def test_ollama_generate_with_stream(self, mock_ollama_service):
        """Test generation with streaming"""
        result = await mock_ollama_service.generate("Test prompt", stream=True)
        mock_ollama_service.generate.assert_called_once_with("Test prompt", stream=True)
    
    @pytest.mark.asyncio
    async def test_ollama_generate_error_handling(self):
        """Test error handling in generate method"""
        service = OllamaService()
        with patch('httpx.AsyncClient') as mock_client:
            mock_client.return_value.__aenter__.return_value.post.side_effect = Exception("Network error")
            
            with pytest.raises(Exception) as exc_info:
                await service.generate("Test prompt")
            assert "Ollama error" in str(exc_info.value)
    
    @pytest.mark.asyncio
    async def test_ollama_generate_connection_error(self):
        """Test connection error handling"""
        service = OllamaService()
        with patch('httpx.AsyncClient') as mock_client:
            mock_client.return_value.__aenter__.return_value.post.side_effect = Exception("Connection failed")
            
            with pytest.raises(Exception):
                await service.generate("Test prompt")


class TestExplanationService:
    """Test Explanation Service"""
    
    def test_explanation_service_initialization(self, mock_ollama_service):
        """Test ExplanationService initialization"""
        with patch('services.explanation_service.OllamaService', return_value=mock_ollama_service):
            service = ExplanationService()
            assert service.llm is not None
    
    @pytest.mark.asyncio
    async def test_generate_explanation_cobol(self, parsed_cobol_data, mock_ollama_service):
        """Test COBOL code explanation generation"""
        with patch('services.explanation_service.OllamaService', return_value=mock_ollama_service):
            service = ExplanationService()
            result = await service.generate_explanation(parsed_cobol_data)
            
            assert 'summary' in result
            assert 'business_logic' in result
            assert 'technical_details' in result
            assert 'modernization_suggestions' in result
            assert 'complexity_assessment' in result
            assert 'raw_explanation' in result
    
    @pytest.mark.asyncio
    async def test_generate_explanation_jcl(self, parsed_jcl_data, mock_ollama_service):
        """Test JCL code explanation generation"""
        with patch('services.explanation_service.OllamaService', return_value=mock_ollama_service):
            service = ExplanationService()
            result = await service.generate_explanation(parsed_jcl_data)
            
            assert 'summary' in result
            assert 'complexity_assessment' in result
    
    def test_build_prompt_cobol(self, parsed_cobol_data):
        """Test prompt building for COBOL"""
        with patch('services.explanation_service.OllamaService'):
            service = ExplanationService()
            prompt = service._build_prompt(parsed_cobol_data, 'COBOL')
            
            assert 'COBOL' in prompt
            assert 'SAMPLECOB' in prompt
            assert 'Analyze this COBOL program' in prompt
    
    def test_build_prompt_jcl(self, parsed_jcl_data):
        """Test prompt building for JCL"""
        with patch('services.explanation_service.OllamaService'):
            service = ExplanationService()
            prompt = service._build_prompt(parsed_jcl_data, 'JCL')
            
            assert 'JCL' in prompt
            assert 'TESTJOB' in prompt
            assert 'Analyze this JCL job' in prompt
    
    def test_extract_summary(self):
        """Test summary extraction"""
        with patch('services.explanation_service.OllamaService'):
            service = ExplanationService()
            text = "First sentence. Second sentence. Third sentence."
            summary = service._extract_summary(text)
            
            assert len(summary) > 0
            assert 'First' in summary
    
    def test_extract_business_logic(self):
        """Test business logic extraction"""
        with patch('services.explanation_service.OllamaService'):
            service = ExplanationService()
            text = "This calculates totals. The business logic is processing customer records."
            logic = service._extract_business_logic(text)
            
            assert len(logic) > 0
    
    def test_extract_modernization_cobol(self):
        """Test modernization suggestions for COBOL"""
        with patch('services.explanation_service.OllamaService'):
            service = ExplanationService()
            suggestions = service._extract_modernization("", 'COBOL')
            
            assert isinstance(suggestions, list)
            assert len(suggestions) > 0
            assert any('Java' in s or 'Python' in s for s in suggestions)
    
    def test_extract_modernization_jcl(self):
        """Test modernization suggestions for JCL"""
        with patch('services.explanation_service.OllamaService'):
            service = ExplanationService()
            suggestions = service._extract_modernization("", 'JCL')
            
            assert isinstance(suggestions, list)
            assert len(suggestions) > 0
            assert any('Airflow' in s or 'Kubernetes' in s for s in suggestions)
    
    def test_assess_complexity_low(self, parsed_cobol_data):
        """Test complexity assessment for simple code"""
        with patch('services.explanation_service.OllamaService'):
            service = ExplanationService()
            
            # Modify data to have low metrics
            parsed_cobol_data['statistics']['variable_count'] = 3
            parsed_cobol_data['statistics']['procedure_count'] = 2
            
            complexity = service._assess_complexity(parsed_cobol_data)
            assert complexity in ['LOW', 'MEDIUM', 'HIGH']
    
    def test_assess_complexity_medium(self, parsed_cobol_data):
        """Test complexity assessment for medium code"""
        with patch('services.explanation_service.OllamaService'):
            service = ExplanationService()
            
            # Modify data to have medium metrics
            parsed_cobol_data['statistics']['variable_count'] = 20
            parsed_cobol_data['statistics']['procedure_count'] = 10
            
            complexity = service._assess_complexity(parsed_cobol_data)
            assert complexity in ['LOW', 'MEDIUM', 'HIGH']
    
    def test_assess_complexity_high(self, parsed_cobol_data):
        """Test complexity assessment for complex code"""
        with patch('services.explanation_service.OllamaService'):
            service = ExplanationService()
            
            # Modify data to have high metrics
            parsed_cobol_data['statistics']['variable_count'] = 100
            parsed_cobol_data['statistics']['procedure_count'] = 50
            
            complexity = service._assess_complexity(parsed_cobol_data)
            assert complexity in ['LOW', 'MEDIUM', 'HIGH']


class TestExplanationServiceIntegration:
    """Integration tests for ExplanationService"""
    
    @pytest.mark.asyncio
    async def test_full_explanation_pipeline_cobol(self, sample_cobol_code, mock_ollama_service):
        """Test full explanation pipeline for COBOL"""
        from parsers.cobol_parser import CobolParser
        
        with patch('services.explanation_service.OllamaService', return_value=mock_ollama_service):
            # Parse code
            parser = CobolParser(sample_cobol_code)
            parsed = parser.parse()
            
            # Generate explanation
            service = ExplanationService()
            explanation = await service.generate_explanation(parsed)
            
            assert explanation['summary'] is not None
            assert explanation['complexity_assessment'] in ['LOW', 'MEDIUM', 'HIGH']
    
    @pytest.mark.asyncio
    async def test_full_explanation_pipeline_jcl(self, sample_jcl_code, mock_ollama_service):
        """Test full explanation pipeline for JCL"""
        from parsers.jcl_parser import JCLParser
        
        with patch('services.explanation_service.OllamaService', return_value=mock_ollama_service):
            # Parse code
            parser = JCLParser(sample_jcl_code)
            parsed = parser.parse()
            
            # Generate explanation
            service = ExplanationService()
            explanation = await service.generate_explanation(parsed)
            
            assert explanation['summary'] is not None
            assert explanation['complexity_assessment'] in ['LOW', 'MEDIUM', 'HIGH']

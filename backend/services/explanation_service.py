from typing import Dict, Any
from .llm_service import OllamaService

class ExplanationService:
    """Service to generate natural language explanations"""
    
    def __init__(self):
        self.llm = OllamaService()
    
    async def generate_explanation(self, parsed_data: Dict[str, Any]) -> Dict[str, Any]:
        """Generate comprehensive explanation"""
        
        code_type = parsed_data.get('type', 'Unknown')
        prompt = self._build_prompt(parsed_data, code_type)
        
        # Generate explanation
        explanation_text = await self.llm.generate(prompt)
        
        return {
            'summary': self._extract_summary(explanation_text),
            'business_logic': self._extract_business_logic(explanation_text),
            'technical_details': explanation_text,
            'modernization_suggestions': self._extract_modernization(explanation_text, code_type),
            'complexity_assessment': self._assess_complexity(parsed_data),
            'raw_explanation': explanation_text
        }
    
    def _build_prompt(self, parsed_data: Dict[str, Any], code_type: str) -> str:
        """Build detailed prompt for LLM"""
        
        if code_type == 'COBOL':
            stats = parsed_data.get('statistics', {})
            metadata = parsed_data.get('metadata', {})
            
            return f"""Analyze this COBOL program:

Program: {metadata.get('program_id', 'Unknown')}
Lines of Code: {stats.get('total_lines', 0)}
Variables: {stats.get('variable_count', 0)}
Procedures: {stats.get('procedure_count', 0)}
I/O Operations: {stats.get('io_operation_count', 0)}

Provide a clear explanation in 3-4 sentences covering:
1. What this program does (business purpose)
2. How it processes data
3. Key operations performed

Be concise and business-friendly."""

        elif code_type == 'JCL':
            stats = parsed_data.get('statistics', {})
            job_info = parsed_data.get('job_info', {})
            utilities = parsed_data.get('utilities', [])
            
            return f"""Analyze this JCL job:

Job: {job_info.get('job_name', 'Unknown')}
Steps: {stats.get('step_count', 0)}
Datasets: {stats.get('dataset_count', 0)}
Utilities: {', '.join([u['utility'] for u in utilities])}

Explain in 3-4 sentences:
1. What this job does
2. The step-by-step process
3. Data transformations

Be concise."""

        return "Analyze this legacy code."
    
    def _extract_summary(self, text: str) -> str:
        """Extract summary"""
        sentences = text.split('.')
        return '. '.join(sentences[:2]) + '.' if sentences else text[:200]
    
    def _extract_business_logic(self, text: str) -> str:
        """Extract business logic"""
        sentences = text.split('.')
        for sent in sentences:
            if any(word in sent.lower() for word in ['purpose', 'business', 'processes', 'handles']):
                return sent.strip() + '.'
        return sentences[0] + '.' if sentences else "See detailed explanation."
    
    def _extract_modernization(self, text: str, code_type: str) -> list:
        """Extract modernization suggestions"""
        if code_type == 'COBOL':
            return [
                "Migrate to Java/Python with business logic preservation",
                "Implement microservices architecture",
                "Use modern databases (PostgreSQL, MongoDB)",
                "Add REST APIs for integration"
            ]
        else:  # JCL
            return [
                "Replace with Apache Airflow or Kubernetes Jobs",
                "Containerize workloads with Docker",
                "Use cloud-native schedulers (AWS Step Functions, Azure Logic Apps)",
                "Implement event-driven architecture"
            ]
    
    def _assess_complexity(self, parsed_data: Dict[str, Any]) -> str:
        """Assess code complexity"""
        stats = parsed_data.get('statistics', {})
        code_type = parsed_data.get('type')
        
        if code_type == 'COBOL':
            var_count = stats.get('variable_count', 0)
            proc_count = stats.get('procedure_count', 0)
            lines = stats.get('total_lines', 0)
            
            score = var_count * 0.5 + proc_count * 2 + lines * 0.1
            
            if score < 100:
                return 'LOW'
            elif score < 300:
                return 'MEDIUM'
            else:
                return 'HIGH'
        
        elif code_type == 'JCL':
            step_count = stats.get('step_count', 0)
            dataset_count = stats.get('dataset_count', 0)
            
            score = step_count * 5 + dataset_count * 2
            
            if score < 30:
                return 'LOW'
            elif score < 80:
                return 'MEDIUM'
            else:
                return 'HIGH'
        
        return 'MEDIUM'
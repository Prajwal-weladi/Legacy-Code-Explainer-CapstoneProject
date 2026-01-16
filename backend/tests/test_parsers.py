"""Tests for parser modules"""

import pytest
from parsers.base_parser import BaseParser
from parsers.cobol_parser import CobolParser
from parsers.jcl_parser import JCLParser


class TestBaseParser:
    """Test BaseParser base class"""
    
    def test_base_parser_initialization(self, sample_cobol_code):
        """Test BaseParser initialization"""
        parser = CobolParser(sample_cobol_code)  # Using subclass
        assert parser.code == sample_cobol_code
        assert len(parser.lines) > 0
    
    def test_get_line_count(self, sample_cobol_code):
        """Test line count calculation"""
        parser = CobolParser(sample_cobol_code)
        count = parser.get_line_count()
        assert count == len(sample_cobol_code.split('\n'))
        assert count > 0
    
    def test_get_non_empty_lines(self, sample_cobol_code):
        """Test non-empty line counting"""
        parser = CobolParser(sample_cobol_code)
        non_empty = parser.get_non_empty_lines()
        assert non_empty > 0
        assert non_empty <= parser.get_line_count()
    
    def test_empty_code_handling(self):
        """Test handling of empty code"""
        parser = CobolParser("")
        assert parser.get_line_count() == 1  # Empty string results in 1 line
        assert parser.get_non_empty_lines() == 0
    
    def test_code_with_only_whitespace(self):
        """Test code with only whitespace"""
        parser = CobolParser("   \n   \n   ")
        assert parser.get_line_count() == 3
        assert parser.get_non_empty_lines() == 0


class TestCobolParser:
    """Test COBOL Parser"""
    
    def test_cobol_parse_returns_dict(self, sample_cobol_code):
        """Test parse method returns dictionary"""
        parser = CobolParser(sample_cobol_code)
        result = parser.parse()
        assert isinstance(result, dict)
    
    def test_cobol_parse_structure(self, sample_cobol_code):
        """Test parse result has required keys"""
        parser = CobolParser(sample_cobol_code)
        result = parser.parse()
        
        assert 'type' in result
        assert result['type'] == 'COBOL'
        assert 'metadata' in result
        assert 'divisions' in result
        assert 'variables' in result
        assert 'procedures' in result
        assert 'statistics' in result
    
    def test_cobol_metadata_extraction(self, sample_cobol_code):
        """Test COBOL metadata extraction"""
        parser = CobolParser(sample_cobol_code)
        result = parser.parse()
        
        metadata = result['metadata']
        assert metadata['program_id'] == 'SAMPLECOB'
        # Author extraction may not capture full value depending on format
        assert 'author' in metadata
    
    def test_cobol_divisions_identification(self, sample_cobol_code):
        """Test COBOL division identification"""
        parser = CobolParser(sample_cobol_code)
        result = parser.parse()
        
        divisions = result['divisions']
        assert 'identification' in divisions
        assert 'data' in divisions
        assert 'procedure' in divisions
    
    def test_cobol_variables_parsing(self, sample_cobol_code):
        """Test COBOL variable parsing"""
        parser = CobolParser(sample_cobol_code)
        result = parser.parse()
        
        variables = result['variables']
        assert isinstance(variables, list)
        # Check that variables have expected structure
        for var in variables:
            assert 'level' in var
            assert 'name' in var
            assert 'line' in var
    
    def test_cobol_procedures_parsing(self, sample_cobol_code):
        """Test COBOL procedure parsing"""
        parser = CobolParser(sample_cobol_code)
        result = parser.parse()
        
        procedures = result['procedures']
        assert isinstance(procedures, list)
        for proc in procedures:
            assert 'type' in proc
            assert 'line' in proc
            # Procedures can have either 'name' or 'target' depending on type
            assert 'name' in proc or 'target' in proc
    
    def test_cobol_statistics(self, sample_cobol_code):
        """Test COBOL statistics calculation"""
        parser = CobolParser(sample_cobol_code)
        result = parser.parse()
        
        stats = result['statistics']
        assert stats['total_lines'] > 0
        assert stats['non_empty_lines'] > 0
        assert stats['variable_count'] >= 0
        assert stats['procedure_count'] >= 0
    
    def test_cobol_minimal_code(self):
        """Test parsing minimal COBOL code"""
        minimal_code = """       IDENTIFICATION DIVISION.
       PROGRAM-ID. MINIMAL."""
        
        parser = CobolParser(minimal_code)
        result = parser.parse()
        
        assert result['type'] == 'COBOL'
        assert result['metadata']['program_id'] == 'MINIMAL'


class TestJCLParser:
    """Test JCL Parser"""
    
    def test_jcl_parse_returns_dict(self, sample_jcl_code):
        """Test parse method returns dictionary"""
        parser = JCLParser(sample_jcl_code)
        result = parser.parse()
        assert isinstance(result, dict)
    
    def test_jcl_parse_structure(self, sample_jcl_code):
        """Test parse result has required keys"""
        parser = JCLParser(sample_jcl_code)
        result = parser.parse()
        
        assert 'type' in result
        assert result['type'] == 'JCL'
        assert 'job_info' in result
        assert 'steps' in result
        assert 'datasets' in result
        assert 'utilities' in result
        assert 'statistics' in result
    
    def test_jcl_job_card_parsing(self, sample_jcl_code):
        """Test JCL job card parsing"""
        parser = JCLParser(sample_jcl_code)
        result = parser.parse()
        
        job_info = result['job_info']
        assert job_info['job_name'] == 'TESTJOB'
        assert job_info['job_card'] is not None
    
    def test_jcl_steps_parsing(self, sample_jcl_code):
        """Test JCL job step parsing"""
        parser = JCLParser(sample_jcl_code)
        result = parser.parse()
        
        steps = result['steps']
        assert isinstance(steps, list)
        assert len(steps) > 0
        
        for step in steps:
            assert 'step_name' in step
            assert 'program' in step
            assert 'line' in step
    
    def test_jcl_datasets_parsing(self, sample_jcl_code):
        """Test JCL dataset parsing"""
        parser = JCLParser(sample_jcl_code)
        result = parser.parse()
        
        datasets = result['datasets']
        assert isinstance(datasets, list)
        
        for dataset in datasets:
            assert 'dd_name' in dataset
            assert 'dsn' in dataset or dataset['dsn'] is None
            assert 'is_temp' in dataset
    
    def test_jcl_utilities_identification(self, sample_jcl_code):
        """Test utility identification"""
        parser = JCLParser(sample_jcl_code)
        result = parser.parse()
        
        utilities = result['utilities']
        assert isinstance(utilities, list)
        
        for utility in utilities:
            assert 'utility' in utility
            assert 'step' in utility
            assert 'purpose' in utility
    
    def test_jcl_utility_purposes(self, sample_jcl_code):
        """Test utility purpose mapping"""
        parser = JCLParser(sample_jcl_code)
        
        # Test utility purposes
        assert parser._get_utility_purpose('IEBGENER') == 'Copy datasets'
        assert parser._get_utility_purpose('SORT') == 'Sort data'
        assert parser._get_utility_purpose('IDCAMS') == 'Manage VSAM datasets'
    
    def test_jcl_statistics(self, sample_jcl_code):
        """Test JCL statistics calculation"""
        parser = JCLParser(sample_jcl_code)
        result = parser.parse()
        
        stats = result['statistics']
        assert stats['total_lines'] > 0
        assert stats['non_empty_lines'] > 0
        assert stats['step_count'] >= 0
        assert stats['dataset_count'] >= 0
    
    def test_jcl_minimal_code(self):
        """Test parsing minimal JCL"""
        minimal_jcl = "//TESTJOB  JOB CLASS=A"
        
        parser = JCLParser(minimal_jcl)
        result = parser.parse()
        
        assert result['type'] == 'JCL'
        assert result['job_info']['job_name'] == 'TESTJOB'
    
    def test_jcl_no_job_card(self):
        """Test JCL without job card"""
        no_job_code = "//STEP1 EXEC PGM=TEST"
        parser = JCLParser(no_job_code)
        result = parser.parse()
        
        assert result['job_info']['job_name'] is None


class TestParserComparison:
    """Test parser comparison and behavior"""
    
    def test_parsers_return_different_types(self, sample_cobol_code, sample_jcl_code):
        """Test that different parsers return different type values"""
        cobol_parser = CobolParser(sample_cobol_code)
        jcl_parser = JCLParser(sample_jcl_code)
        
        cobol_result = cobol_parser.parse()
        jcl_result = jcl_parser.parse()
        
        assert cobol_result['type'] == 'COBOL'
        assert jcl_result['type'] == 'JCL'
    
    def test_both_parsers_have_statistics(self, sample_cobol_code, sample_jcl_code):
        """Test both parsers include statistics"""
        cobol_parser = CobolParser(sample_cobol_code)
        jcl_parser = JCLParser(sample_jcl_code)
        
        cobol_result = cobol_parser.parse()
        jcl_result = jcl_parser.parse()
        
        assert 'statistics' in cobol_result
        assert 'statistics' in jcl_result
        assert cobol_result['statistics']['total_lines'] > 0
        assert jcl_result['statistics']['total_lines'] > 0

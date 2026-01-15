import re
from typing import Dict, List, Any
from .base_parser import BaseParser

class CobolParser(BaseParser):
    """Enhanced COBOL parser with detailed analysis"""
    
    def parse(self) -> Dict[str, Any]:
        """Main parsing method"""
        metadata = self._extract_metadata()
        variables = self._parse_variables()
        procedures = self._parse_procedures()
        io_operations = self._parse_io_operations()
        
        return {
            'type': 'COBOL',
            'metadata': metadata,
            'divisions': self._parse_divisions(),
            'variables': variables,
            'procedures': procedures,
            'io_operations': io_operations,
            'statistics': {
                'total_lines': self.get_line_count(),
                'non_empty_lines': self.get_non_empty_lines(),
                'variable_count': len(variables),
                'procedure_count': len([p for p in procedures if p['type'] in ['section', 'paragraph']]),
                'io_operation_count': len(io_operations)
            }
        }
    
    def _extract_metadata(self) -> Dict[str, str]:
        """Extract program metadata"""
        metadata = {
            'program_id': None,
            'author': None,
            'date_written': None
        }
        
        for line in self.lines[:50]:
            upper_line = line.upper().strip()
            
            if 'PROGRAM-ID' in upper_line:
                match = re.search(r'PROGRAM-ID[.\s]+([A-Z0-9-]+)', upper_line)
                if match:
                    metadata['program_id'] = match.group(1)
            
            elif 'AUTHOR' in upper_line:
                parts = line.split('.')
                if parts:
                    author = parts[0].replace('AUTHOR', '').replace('author', '').strip()
                    metadata['author'] = author
            
            elif 'DATE-WRITTEN' in upper_line:
                parts = line.split('.')
                if parts:
                    date = parts[0].replace('DATE-WRITTEN', '').replace('date-written', '').strip()
                    metadata['date_written'] = date
        
        return metadata
    
    def _parse_divisions(self) -> Dict[str, int]:
        """Identify divisions and their line numbers"""
        divisions = {}
        
        for i, line in enumerate(self.lines, 1):
            upper_line = line.upper().strip()
            
            if 'IDENTIFICATION DIVISION' in upper_line:
                divisions['identification'] = i
            elif 'ENVIRONMENT DIVISION' in upper_line:
                divisions['environment'] = i
            elif 'DATA DIVISION' in upper_line:
                divisions['data'] = i
            elif 'PROCEDURE DIVISION' in upper_line:
                divisions['procedure'] = i
        
        return divisions
    
    def _parse_variables(self) -> List[Dict[str, Any]]:
        """Parse data division variables"""
        variables = []
        in_data_division = False
        
        for i, line in enumerate(self.lines, 1):
            upper_line = line.upper().strip()
            
            if 'DATA DIVISION' in upper_line:
                in_data_division = True
                continue
            
            if 'PROCEDURE DIVISION' in upper_line:
                break
            
            if in_data_division:
                match = re.match(r'^\s*(\d{2})\s+([A-Z0-9-]+)', line, re.IGNORECASE)
                if match:
                    level = match.group(1)
                    name = match.group(2)
                    
                    pic_match = re.search(r'PIC\s+([A-Z0-9()]+)', upper_line)
                    picture = pic_match.group(1) if pic_match else None
                    
                    value_match = re.search(r'VALUE\s+(["\']?[^."\']+["\']?)', line, re.IGNORECASE)
                    value = value_match.group(1).strip('\'"') if value_match else None
                    
                    variables.append({
                        'level': level,
                        'name': name,
                        'picture': picture,
                        'value': value,
                        'line': i
                    })
        
        return variables
    
    def _parse_procedures(self) -> List[Dict[str, Any]]:
        """Parse procedure division"""
        procedures = []
        in_procedure_division = False
        
        for i, line in enumerate(self.lines, 1):
            upper_line = line.upper().strip()
            
            if 'PROCEDURE DIVISION' in upper_line:
                in_procedure_division = True
                continue
            
            if in_procedure_division:
                # Section
                if ' SECTION' in upper_line and upper_line.endswith('.'):
                    section_name = upper_line.split()[0]
                    procedures.append({
                        'type': 'section',
                        'name': section_name,
                        'line': i
                    })
                
                # Paragraph
                elif re.match(r'^[A-Z][A-Z0-9-]*\.$', upper_line):
                    para_name = upper_line[:-1]
                    procedures.append({
                        'type': 'paragraph',
                        'name': para_name,
                        'line': i
                    })
                
                # PERFORM
                elif 'PERFORM' in upper_line:
                    perform_match = re.search(r'PERFORM\s+([A-Z0-9-]+)', upper_line)
                    if perform_match:
                        procedures.append({
                            'type': 'perform',
                            'target': perform_match.group(1),
                            'line': i,
                            'is_loop': 'UNTIL' in upper_line or 'TIMES' in upper_line
                        })
        
        return procedures
    
    def _parse_io_operations(self) -> List[Dict[str, Any]]:
        """Parse I/O operations"""
        io_ops = []
        io_keywords = ['READ', 'WRITE', 'OPEN', 'CLOSE', 'ACCEPT', 'DISPLAY']
        
        for i, line in enumerate(self.lines, 1):
            upper_line = line.upper().strip()
            
            for keyword in io_keywords:
                if f' {keyword} ' in f' {upper_line} ':
                    io_ops.append({
                        'operation': keyword,
                        'line': i,
                        'statement': line.strip()
                    })
                    break
        
        return io_ops
import re
from typing import Dict, List, Any
from .base_parser import BaseParser

class JCLParser(BaseParser):
    """Enhanced JCL parser"""
    
    def parse(self) -> Dict[str, Any]:
        """Main parsing method"""
        job_info = self._parse_job_card()
        steps = self._parse_steps()
        datasets = self._parse_datasets()
        utilities = self._identify_utilities()
        
        return {
            'type': 'JCL',
            'job_info': job_info,
            'steps': steps,
            'datasets': datasets,
            'utilities': utilities,
            'statistics': {
                'total_lines': self.get_line_count(),
                'non_empty_lines': self.get_non_empty_lines(),
                'step_count': len(steps),
                'dataset_count': len(datasets),
                'utility_count': len(utilities)
            }
        }
    
    def _parse_job_card(self) -> Dict[str, Any]:
        """Parse JOB card"""
        for line in self.lines[:20]:
            if line.startswith('//') and 'JOB' in line.upper():
                match = re.match(r'^//([A-Z0-9]+)\s+JOB', line)
                if match:
                    return {
                        'job_name': match.group(1),
                        'job_card': line.strip()
                    }
        return {'job_name': None, 'job_card': None}
    
    def _parse_steps(self) -> List[Dict[str, Any]]:
        """Parse job steps"""
        steps = []
        
        for i, line in enumerate(self.lines, 1):
            if line.startswith('//') and 'EXEC' in line.upper():
                match = re.match(r'^//([A-Z0-9]+)\s+EXEC\s+(?:PGM=)?([A-Z0-9]+)', line)
                if match:
                    steps.append({
                        'step_name': match.group(1),
                        'program': match.group(2),
                        'line': i,
                        'is_proc': 'PROC=' in line.upper()
                    })
        
        return steps
    
    def _parse_datasets(self) -> List[Dict[str, Any]]:
        """Parse DD statements"""
        datasets = []
        
        for i, line in enumerate(self.lines, 1):
            if line.startswith('//') and ' DD ' in line.upper():
                dd_match = re.match(r'^//([A-Z0-9]+)\s+DD', line)
                dsn_match = re.search(r'DSN=([A-Z0-9.()&]+)', line)
                disp_match = re.search(r'DISP=\(([^)]+)\)', line)
                
                if dd_match:
                    datasets.append({
                        'dd_name': dd_match.group(1),
                        'dsn': dsn_match.group(1) if dsn_match else None,
                        'disposition': disp_match.group(1) if disp_match else None,
                        'is_temp': '&&' in line,
                        'line': i
                    })
        
        return datasets
    
    def _identify_utilities(self) -> List[Dict[str, str]]:
        """Identify system utilities"""
        utilities = []
        known_utilities = ['IEBGENER', 'SORT', 'IDCAMS', 'IEFBR14', 'IEBCOPY', 'IEBUPDTE']
        
        for step in self._parse_steps():
            if step['program'] in known_utilities:
                utilities.append({
                    'utility': step['program'],
                    'step': step['step_name'],
                    'line': step['line'],
                    'purpose': self._get_utility_purpose(step['program'])
                })
        
        return utilities
    
    def _get_utility_purpose(self, utility: str) -> str:
        """Get utility purpose"""
        purposes = {
            'IEBGENER': 'Copy datasets',
            'SORT': 'Sort data',
            'IDCAMS': 'Manage VSAM datasets',
            'IEFBR14': 'Allocate/delete datasets',
            'IEBCOPY': 'Copy partitioned datasets',
            'IEBUPDTE': 'Update sequential datasets'
        }
        return purposes.get(utility, 'Unknown')
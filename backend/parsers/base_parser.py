from abc import ABC, abstractmethod
from typing import Dict, Any

class BaseParser(ABC):
    """Base class for all legacy code parsers"""
    
    def __init__(self, code: str):
        self.code = code
        self.lines = code.split('\n')
        self.parsed_data = {}
    
    @abstractmethod
    def parse(self) -> Dict[str, Any]:
        """Parse the code and return structured data"""
        pass
    
    def get_line_count(self) -> int:
        """Get total lines of code"""
        return len(self.lines)
    
    def get_non_empty_lines(self) -> int:
        """Get non-empty lines"""
        return len([line for line in self.lines if line.strip()])
import httpx
import json
from typing import Dict, Any, Optional
from config import settings

class OllamaService:
    """Async Ollama service"""
    
    def __init__(self):
        self.host = settings.ollama_host
        self.model = settings.ollama_model
        self.timeout = settings.ollama_timeout
    
    async def generate(self, prompt: str, stream: bool = False) -> str:
        """Generate response from Ollama asynchronously"""
        url = f"{self.host}/api/generate"
        payload = {
            "model": self.model,
            "prompt": prompt,
            "stream": stream
        }
        
        try:
            async with httpx.AsyncClient(timeout=self.timeout) as client:
                response = await client.post(url, json=payload)
                response.raise_for_status()
                
                data = response.json()
                return data.get('response', '')
        
        except httpx.RequestError as e:
            raise Exception(f"Ollama connection error: {str(e)}")
        except Exception as e:
            raise Exception(f"Ollama error: {str(e)}")
    
    async def is_available(self) -> bool:
        """Check if Ollama is running"""
        try:
            async with httpx.AsyncClient(timeout=5) as client:
                response = await client.get(f"{self.host}/api/tags")
                return response.status_code == 200
        except:
            return False
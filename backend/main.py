from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware
from fastapi.staticfiles import StaticFiles
from fastapi.responses import FileResponse
from contextlib import asynccontextmanager

from config import settings
from api.routes import router
from services.llm_service import OllamaService

@asynccontextmanager
async def lifespan(app: FastAPI):
    """Startup and shutdown events"""
    # Startup
    print("Legacy Code Explainer Starting...")
    print(f"Ollama Host: {settings.ollama_host}")
    print(f"Ollama Model: {settings.ollama_model}")
    
    ollama = OllamaService()
    if await ollama.is_available():
        print("Ollama is running!")
    else:
        print("Warning: Ollama is not running. Start it with 'ollama serve'")
    
    yield
    
    # Shutdown
    print("Shutting down...")

# Create FastAPI app
app = FastAPI(
    title=settings.app_name,
    version=settings.version,
    description="AI-powered legacy code analysis and modernization tool",
    lifespan=lifespan
)

# CORS middleware
app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.cors_origin,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Include API routes
app.include_router(router, prefix="/api", tags=["Analysis"])

# Serve frontend
app.mount("/static", StaticFiles(directory="../frontend"), name="static")

@app.get("/")
async def read_index():
    """Serve frontend"""
    return FileResponse("../frontend/index.html")

@app.get("/docs")
async def get_docs():
    """Redirect to interactive API docs"""
    return {"message": "Visit /docs for interactive API documentation"}

if __name__ == "__main__":
    import uvicorn
    uvicorn.run(
        "main:app",
        host="0.0.0.0",
        port=8000,
        reload=True,  # Auto-reload on code changes
        log_level="info"
    )
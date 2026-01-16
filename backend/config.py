from pydantic_settings import BaseSettings
from pathlib import Path
from typing import Set

class Settings(BaseSettings):
    app_name: str = "Legacy Code Explorer"
    version: str = "1.0.0"

    base_dir: Path = Path(__file__).parent.parent
    upload_folder: Path = base_dir/ "uploads"
    output_folder: Path = base_dir/ "outputs"

    ollama_host: str = "http://localhost:11434"
    ollama_model: str = "codellama"
    ollama_timeout: int = 120

    max_file_size: int = 5 * 1024 * 1024
    allowed_extensions: Set[str] = {"cbl", "cob", "jcl", "txt"}

    cors_origin: list = ["*"]

    mongodb_url: str = "mongodb://localhost:27017"
    mongodb_db_name: str = "legacy_code_db"
    cache_enabled: bool = True
    cache_ttl_days: int = 30

    class Config:
        env_file = ".env"
        case_sensitive = False

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.upload_folder.mkdir(exist_ok=True)
        self.output_folder.mkdir(exist_ok=True)

settings = Settings()

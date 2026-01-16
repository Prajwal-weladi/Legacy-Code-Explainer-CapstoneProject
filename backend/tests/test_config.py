"""Tests for config module"""

import pytest
from pathlib import Path
from config import Settings, settings


class TestSettings:
    """Test Settings class"""
    
    def test_settings_defaults(self):
        """Test default settings values"""
        assert settings.app_name == "Legacy Code Explorer"
        assert settings.version == "1.0.0"
        assert settings.ollama_model == "codellama"
        assert settings.ollama_timeout == 120
    
    def test_settings_folders_creation(self, mock_settings):
        """Test that upload and output folders are created"""
        assert mock_settings.upload_folder.exists()
        assert mock_settings.output_folder.exists()
    
    def test_settings_allowed_extensions(self, mock_settings):
        """Test allowed file extensions"""
        assert "cbl" in mock_settings.allowed_extensions
        assert "cob" in mock_settings.allowed_extensions
        assert "jcl" in mock_settings.allowed_extensions
        assert "txt" in mock_settings.allowed_extensions
    
    def test_settings_max_file_size(self, mock_settings):
        """Test max file size configuration"""
        assert mock_settings.max_file_size == 5 * 1024 * 1024
    
    def test_settings_cache_configuration(self, mock_settings):
        """Test cache settings"""
        assert mock_settings.cache_enabled is True
        assert mock_settings.cache_ttl_days == 30
    
    def test_settings_mongodb_configuration(self, mock_settings):
        """Test MongoDB settings"""
        assert mock_settings.mongodb_url == "mongodb://localhost:27017"
        assert mock_settings.mongodb_db_name == "test_db"
    
    def test_settings_cors_origins(self, mock_settings):
        """Test CORS origins"""
        assert "*" in mock_settings.cors_origin
    
    def test_settings_ollama_configuration(self, mock_settings):
        """Test Ollama service configuration"""
        assert mock_settings.ollama_host == "http://localhost:11434"
        assert mock_settings.ollama_model == "codellama"
    
    def test_settings_base_directory(self, mock_settings):
        """Test base directory path"""
        assert mock_settings.base_dir.exists()
        assert isinstance(mock_settings.base_dir, Path)
    
    def test_settings_initialization_with_kwargs(self, temp_dir):
        """Test Settings initialization with custom kwargs"""
        custom_settings = Settings(
            app_name="Custom App",
            version="2.0.0",
            base_dir=temp_dir,
            upload_folder=temp_dir / "uploads",
            output_folder=temp_dir / "outputs"
        )
        assert custom_settings.app_name == "Custom App"
        assert custom_settings.version == "2.0.0"
    
    def test_settings_env_file_support(self, mock_settings):
        """Test environment file configuration support"""
        assert mock_settings.Config.env_file == ".env"
        assert mock_settings.Config.case_sensitive is False

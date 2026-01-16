# Legacy Code Explainer

An AI-powered FastAPI application that analyzes legacy COBOL and JCL code, providing detailed explanations, complexity assessments, and modernization suggestions.

## 🎯 Overview

Legacy Code Explainer transforms complex legacy code into human-readable explanations using advanced AI models. It automatically detects code type, parses program structures, and generates comprehensive analyses including business logic summaries, technical details, and modernization recommendations.

### Key Features

✅ **Multi-Language Support**
- COBOL program analysis
- JCL job analysis
- Automatic code type detection

✅ **Comprehensive Parsing**
- Extract metadata (PROGRAM-ID, AUTHOR, DATE-WRITTEN)
- Identify program divisions and sections
- Parse variables with PIC clauses
- Analyze procedures and I/O operations
- Extract job steps, datasets, and utilities

✅ **AI-Powered Explanations**
- Natural language code summaries
- Business logic extraction
- Technical detail analysis
- Complexity assessment (LOW/MEDIUM/HIGH)
- Modernization suggestions

✅ **Production Ready**
- 97% code coverage with 94 tests
- Async/await support
- MongoDB caching support
- CORS enabled
- Error handling and recovery

---

## 📋 Prerequisites

- **Python**: 3.8+
- **Ollama**: Running locally (for AI explanations)
  - Download: https://ollama.ai
  - Run: `ollama serve`
  - Pull model: `ollama pull codellama`

---

## 🚀 Installation

### 1. Clone Repository
```bash
cd "Legacy code explainer"
```

### 2. Create Virtual Environment
```bash
python -m venv myenv
# Windows
myenv\Scripts\activate
# Linux/Mac
source myenv/bin/activate
```

### 3. Install Dependencies
```bash
cd backend
pip install -r requirements.txt
```

### 4. Configure Environment
Create `.env` file in backend directory:
```env
OLLAMA_HOST=http://localhost:11434
OLLAMA_MODEL=codellama
OLLAMA_TIMEOUT=120
MONGODB_URL=mongodb://localhost:27017
MONGODB_DB_NAME=legacy_code_db
CACHE_ENABLED=true
```

### 5. Start Ollama (in separate terminal)
```bash
ollama serve
```

### 6. Run Application
```bash
cd backend
python main.py
```

Application starts at: `http://localhost:8000`

---

## 📚 Project Structure

```
Legacy code explainer/
├── backend/                          # FastAPI backend
│   ├── main.py                      # Application entry point
│   ├── config.py                    # Settings and configuration
│   ├── requirements.txt              # Python dependencies
│   ├── pytest.ini                    # Test configuration
│   │
│   ├── api/
│   │   └── routes.py                # API endpoints
│   │
│   ├── models/
│   │   └── schemas.py               # Pydantic data models
│   │
│   ├── parsers/
│   │   ├── base_parser.py           # Base parser class
│   │   ├── cobol_parser.py          # COBOL parser
│   │   └── jcl_parser.py            # JCL parser
│   │
│   ├── services/
│   │   ├── llm_service.py           # Ollama integration
│   │   └── explanation_service.py   # Explanation generation
│   │
│   ├── tests/                       # Comprehensive test suite
│   │   ├── conftest.py              # Test fixtures
│   │   ├── test_config.py           # 11 config tests
│   │   ├── test_parsers.py          # 28 parser tests
│   │   ├── test_services.py         # 16 service tests
│   │   ├── test_routes.py           # 24 API tests
│   │   ├── test_integration.py      # 15 integration tests
│   │   └── README.md                # Test documentation
│   │
│   └── htmlcov/                     # Coverage reports
│
├── frontend/                         # Web UI
│   ├── index.html
│   ├── app.js
│   └── style.css
│
├── uploads/                          # Uploaded files
└── outputs/                          # Generated reports
```

---

## 🔌 API Endpoints

### Health Check
```http
GET /api/health
```
**Response**: Service status and Ollama availability
```json
{
  "status": "healthy",
  "ollama_available": true,
  "ollama_host": "http://localhost:11434",
  "ollama_model": "codellama",
  "timestamp": "2026-01-16T10:30:00"
}
```

### Analyze Code
```http
POST /api/analyze
Content-Type: multipart/form-data

file: [COBOL/JCL file]
```
**Response**: Complete analysis with parsing and explanation
```json
{
  "success": true,
  "filename": "program.cob",
  "code_type": "cobol",
  "parsed_data": {
    "type": "COBOL",
    "metadata": { "program_id": "MYPROG" },
    "statistics": { "total_lines": 150, "variable_count": 25 }
  },
  "explanation": {
    "summary": "Program description...",
    "business_logic": "What it does...",
    "technical_details": "How it works...",
    "modernization_suggestions": [...],
    "complexity_assessment": "MEDIUM"
  },
  "processing_time": 3.45
}
```

### Parse Code Only
```http
POST /api/parse
Content-Type: multipart/form-data

file: [COBOL/JCL file]
```
**Response**: Parsing results without AI explanation
```json
{
  "success": true,
  "filename": "program.cob",
  "code_type": "cobol",
  "parsed_data": { ... }
}
```

---

## 💻 Usage Examples

### Python Client
```python
import httpx

# Analyze a COBOL file
with open("program.cob", "rb") as f:
    files = {"file": f}
    response = httpx.post("http://localhost:8000/api/analyze", files=files)
    result = response.json()
    
    print(f"Type: {result['code_type']}")
    print(f"Summary: {result['explanation']['summary']}")
    print(f"Complexity: {result['explanation']['complexity_assessment']}")
```

### CURL
```bash
# Analyze file
curl -X POST -F "file=@program.cob" http://localhost:8000/api/analyze

# Health check
curl http://localhost:8000/api/health
```

### Web Interface
1. Navigate to http://localhost:8000
2. Upload COBOL or JCL file
3. View analysis and recommendations

---

## 🧪 Testing

### Run All Tests
```bash
cd backend
pytest tests/ -v
```

### Run with Coverage Report
```bash
pytest --cov=. --cov-report=html --cov-report=term
```

### View Coverage Report
```bash
# Opens in browser
start htmlcov/index.html
```

### Run Specific Tests
```bash
# All parser tests
pytest tests/test_parsers.py -v

# COBOL parser only
pytest tests/test_parsers.py::TestCobolParser -v

# Tests matching pattern
pytest tests/ -k "cobol" -v
```

### Test Statistics
- **Total Tests**: 94
- **Pass Rate**: 100%
- **Code Coverage**: 97%
- **Execution Time**: ~2.5 minutes

### Test Coverage by Module
| Module | Coverage | Status |
|--------|----------|--------|
| config.py | 100% | ✅ |
| models/schemas.py | 100% | ✅ |
| parsers/cobol_parser.py | 100% | ✅ |
| parsers/jcl_parser.py | 100% | ✅ |
| services/llm_service.py | 97% | ✅ |
| api/routes.py | 92% | ✅ |
| services/explanation_service.py | 89% | ✅ |

---

## ⚙️ Configuration

### Environment Variables (`.env` file)

```env
# Ollama Service
OLLAMA_HOST=http://localhost:11434          # Ollama server URL
OLLAMA_MODEL=codellama                      # Model name
OLLAMA_TIMEOUT=120                          # Request timeout (seconds)

# File Upload
MAX_FILE_SIZE=5242880                       # 5MB in bytes
ALLOWED_EXTENSIONS=cbl,cob,jcl,txt         # Allowed file types

# MongoDB
MONGODB_URL=mongodb://localhost:27017       # MongoDB connection
MONGODB_DB_NAME=legacy_code_db             # Database name

# Cache
CACHE_ENABLED=true                          # Enable caching
CACHE_TTL_DAYS=30                          # Cache TTL

# CORS
CORS_ORIGIN=*                              # CORS allowed origins
```

### Settings File (`config.py`)
All defaults defined in `backend/config.py`. Override with environment variables.

---

## 📖 Documentation Files

| File | Purpose |
|------|---------|
| `README.md` | This file - project overview |
| `TEST_COVERAGE_REPORT.md` | Detailed test coverage analysis |
| `COMPLETE_TEST_INVENTORY.md` | All 94 tests listed with descriptions |
| `QUICK_TEST_GUIDE.md` | Quick reference for testing |
| `TEST_SUITE_SUMMARY.md` | Test implementation summary |
| `README_TESTS.md` | Testing setup and usage |
| `backend/tests/README.md` | Detailed test documentation |

---

## 🔍 How It Works

### COBOL Analysis Pipeline
```
Upload COBOL File
    ↓
Detect Type (.cob, .cbl)
    ↓
Parse Structure
    ├─ Identification Division → Extract metadata
    ├─ Environment Division → I/O operations
    ├─ Data Division → Variables with PIC clauses
    └─ Procedure Division → Sections, paragraphs, PERFORM
    ↓
Build Prompt for AI Model
    ↓
Generate Explanation via Ollama
    ├─ Business purpose summary
    ├─ Logic explanation
    ├─ Technical details
    ├─ Complexity assessment
    └─ Modernization suggestions
    ↓
Return Complete Analysis
```

### JCL Analysis Pipeline
```
Upload JCL File
    ↓
Detect Type (.jcl)
    ↓
Parse Structure
    ├─ Job Card → Job name and parameters
    ├─ EXEC Statements → Job steps
    ├─ DD Statements → Datasets and allocations
    └─ Utilities → Identify IEBGENER, SORT, etc.
    ↓
Build Prompt for AI Model
    ↓
Generate Explanation via Ollama
    ├─ Job purpose summary
    ├─ Step-by-step process
    ├─ Data transformations
    ├─ Complexity assessment
    └─ Modernization suggestions
    ↓
Return Complete Analysis
```

---



## 📞 Quick Start Commands

```bash
# Full setup
python -m venv myenv
myenv\Scripts\activate
cd backend
pip install -r requirements.txt

# Start Ollama (separate terminal)
ollama serve

# Run application
python main.py

# Test in another terminal
pytest tests/ -v --cov=. --cov-report=html

# View coverage
start htmlcov/index.html

# Access application
# API: http://localhost:8000/api
# Docs: http://localhost:8000/docs
# UI: http://localhost:8000
```

---

**Status**: ✅ Production Ready  
**Tests**: ✅ 94/94 Passing  
**Coverage**: ✅ 97%  
**Last Updated**: January 16, 2026

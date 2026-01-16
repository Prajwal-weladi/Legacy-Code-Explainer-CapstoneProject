"""Pytest configuration and fixtures"""

import pytest
import tempfile
from pathlib import Path
from unittest.mock import AsyncMock, MagicMock, patch
import sys
import os

# Add backend to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from config import Settings
from models.schemas import HealthResponse, Statistics, ParsedData, Explanation, AnalyzeResponse
from fastapi.testclient import TestClient


@pytest.fixture
def temp_dir():
    """Create a temporary directory for testing"""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield Path(tmpdir)


@pytest.fixture
def mock_settings(temp_dir):
    """Mock settings for testing"""
    return Settings(
        app_name="Test App",
        version="1.0.0",
        base_dir=temp_dir,
        upload_folder=temp_dir / "uploads",
        output_folder=temp_dir / "outputs",
        ollama_host="http://localhost:11434",
        ollama_model="codellama",
        ollama_timeout=120,
        max_file_size=5 * 1024 * 1024,
        allowed_extensions={"cbl", "cob", "jcl", "txt"},
        cors_origin=["*"]
    )


@pytest.fixture
def sample_cobol_code():
    """Sample COBOL program for testing"""
    return """       IDENTIFICATION DIVISION.
       PROGRAM-ID. SAMPLECOB.
       AUTHOR. TEST AUTHOR.
       DATE-WRITTEN. 01/15/2024.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO WS-INPUT-FILE
               ORGANIZATION IS LINE SEQUENTIAL.
       
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE.
       01  INPUT-RECORD.
           05  CUST-ID        PIC 9(5).
           05  CUST-NAME      PIC X(30).
           05  AMOUNT         PIC 9(10)V99.
       
       WORKING-STORAGE SECTION.
       01  WS-VARIABLES.
           05  WS-INPUT-FILE  PIC X(50) VALUE 'input.txt'.
           05  WS-TOTAL       PIC 9(10)V99 VALUE 0.
           05  WS-COUNT       PIC 9(5) VALUE 0.
           05  WS-EOF         PIC X VALUE 'N'.
       
       PROCEDURE DIVISION.
       MAIN-SECTION.
           OPEN INPUT INPUT-FILE.
           
           PERFORM READ-RECORDS UNTIL WS-EOF = 'Y'.
           
           CLOSE INPUT-FILE.
           STOP RUN.
       
       READ-RECORDS SECTION.
           READ INPUT-FILE
               AT END SET WS-EOF TO 'Y'
               NOT AT END
                   ADD AMOUNT TO WS-TOTAL
                   ADD 1 TO WS-COUNT
           END-READ.
"""


@pytest.fixture
def sample_jcl_code():
    """Sample JCL for testing"""
    return """//TESTJOB  JOB (ACCOUNT,DEPT),'TEST JOB',CLASS=A,
//             MSGCLASS=X,NOTIFY=&SYSUID
//*
//* Test JCL Job
//*
//STEP1    EXEC PGM=IEBGENER
//SYSPRINT DD   SYSOUT=*
//SYSUT1   DD   DSN=INPUT.DATASET,
//             DISP=SHR
//SYSUT2   DD   DSN=OUTPUT.DATASET,
//             DISP=(NEW,CATLG,DELETE),
//             UNIT=SYSDA,
//             SPACE=(TRK,(10,5))
//
//STEP2    EXEC PGM=SORT
//SYSOUT   DD   SYSOUT=*
//SORTIN   DD   DSN=OUTPUT.DATASET,
//             DISP=SHR
//SORTOUT  DD   DSN=SORTED.DATASET,
//             DISP=(NEW,CATLG,DELETE)
//SYSIN    DD   *
  SORT FIELDS=(1,10,CH,A)
/*
"""


@pytest.fixture
def mock_ollama_service():
    """Mock Ollama service"""
    service = AsyncMock()
    service.is_available = AsyncMock(return_value=True)
    service.generate = AsyncMock(return_value="This is a test explanation.")
    return service


@pytest.fixture
def parsed_cobol_data():
    """Sample parsed COBOL data"""
    return {
        'type': 'COBOL',
        'metadata': {
            'program_id': 'SAMPLECOB',
            'author': 'TEST AUTHOR',
            'date_written': '01/15/2024'
        },
        'divisions': {
            'identification': 1,
            'environment': 5,
            'data': 10,
            'procedure': 20
        },
        'variables': [
            {
                'level': '01',
                'name': 'INPUT-RECORD',
                'picture': None,
                'value': None,
                'line': 14
            }
        ],
        'procedures': [
            {'type': 'section', 'name': 'MAIN-SECTION', 'line': 27},
            {'type': 'section', 'name': 'READ-RECORDS', 'line': 36}
        ],
        'io_operations': [],
        'statistics': {
            'total_lines': 45,
            'non_empty_lines': 35,
            'variable_count': 4,
            'procedure_count': 2,
            'io_operation_count': 0
        }
    }


@pytest.fixture
def parsed_jcl_data():
    """Sample parsed JCL data"""
    return {
        'type': 'JCL',
        'job_info': {
            'job_name': 'TESTJOB',
            'job_card': "//TESTJOB  JOB (ACCOUNT,DEPT),'TEST JOB',CLASS=A"
        },
        'steps': [
            {
                'step_name': 'STEP1',
                'program': 'IEBGENER',
                'line': 5,
                'is_proc': False
            },
            {
                'step_name': 'STEP2',
                'program': 'SORT',
                'line': 15,
                'is_proc': False
            }
        ],
        'datasets': [
            {
                'dd_name': 'SYSUT1',
                'dsn': 'INPUT.DATASET',
                'disposition': 'SHR',
                'is_temp': False,
                'line': 7
            }
        ],
        'utilities': [
            {
                'utility': 'IEBGENER',
                'step': 'STEP1',
                'line': 5,
                'purpose': 'Copy datasets'
            }
        ],
        'statistics': {
            'total_lines': 20,
            'non_empty_lines': 16,
            'step_count': 2,
            'dataset_count': 1,
            'utility_count': 1
        }
    }


@pytest.fixture
def explanation_data():
    """Sample explanation data"""
    return {
        'summary': 'This program processes customer records and calculates totals.',
        'business_logic': 'The program reads customer data from an input file.',
        'technical_details': 'A comprehensive technical explanation of how the code works.',
        'modernization_suggestions': [
            'Migrate to Java/Python',
            'Implement microservices architecture'
        ],
        'complexity_assessment': 'LOW',
        'raw_explanation': 'Full explanation text here.'
    }


@pytest.fixture
def test_client():
    """FastAPI test client"""
    from main import app
    return TestClient(app)


@pytest.fixture
def mock_file_upload_cobol():
    """Mock COBOL file upload"""
    from fastapi import UploadFile
    from io import BytesIO
    
    file = MagicMock(spec=UploadFile)
    file.filename = "test.cob"
    file.read = AsyncMock(return_value=b"IDENTIFICATION DIVISION. PROGRAM-ID. TEST.")
    return file


@pytest.fixture
def mock_file_upload_jcl():
    """Mock JCL file upload"""
    from fastapi import UploadFile
    from io import BytesIO
    
    file = MagicMock(spec=UploadFile)
    file.filename = "test.jcl"
    file.read = AsyncMock(return_value=b"//TEST JOB CLASS=A")
    return file

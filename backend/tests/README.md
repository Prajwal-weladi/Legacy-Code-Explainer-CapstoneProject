# Pytest Coverage Report Script

This directory contains comprehensive test coverage for the Legacy Code Explainer project.

## Test Structure

```
tests/
├── conftest.py              # Pytest fixtures and configuration
├── test_config.py          # Configuration module tests
├── test_parsers.py         # Parser module tests (COBOL, JCL, Base)
├── test_services.py        # Service module tests (Ollama, Explanation)
├── test_routes.py          # API endpoint tests
├── test_integration.py     # End-to-end integration tests
└── pytest.ini              # Pytest configuration
```

## Test Coverage Summary

### Unit Tests (test_config.py)
- Settings initialization and validation
- Environment configuration
- File system operations

### Parser Tests (test_parsers.py)
- **BaseParser**: Line counting, code analysis
- **CobolParser**: Metadata, divisions, variables, procedures
- **JCLParser**: Job cards, steps, datasets, utilities
- Edge cases and minimal code handling

### Service Tests (test_services.py)
- **OllamaService**: Connection, generation, availability checks
- **ExplanationService**: Prompt building, text extraction, complexity assessment
- Error handling and recovery

### Route Tests (test_routes.py)
- **Health Endpoint**: Status checking, response structure
- **Analyze Endpoint**: File upload, processing, response format
- **Parse Endpoint**: Code parsing without explanation
- Schema validation
- Error handling

### Integration Tests (test_integration.py)
- End-to-end COBOL analysis pipeline
- End-to-end JCL analysis pipeline
- Service integration flows
- File type detection
- Data consistency
- Concurrent operations
- Error recovery

## Running Tests

### Install Test Dependencies
```bash
pip install -r requirements.txt
```

### Run All Tests
```bash
pytest
```

### Run with Coverage Report
```bash
pytest --cov=. --cov-report=html --cov-report=term
```

### Run Specific Test File
```bash
pytest tests/test_parsers.py -v
```

### Run Specific Test Class
```bash
pytest tests/test_parsers.py::TestCobolParser -v
```

### Run Specific Test Function
```bash
pytest tests/test_parsers.py::TestCobolParser::test_cobol_parse_returns_dict -v
```

### Run Tests Matching Pattern
```bash
pytest -k "cobol" -v
```

### Run Only Async Tests
```bash
pytest -m asyncio -v
```

### Run with Detailed Output
```bash
pytest -v --tb=long
```

## Coverage Report Generation

### HTML Coverage Report
```bash
pytest --cov=. --cov-report=html
# Opens as coverage_html_report/index.html
```

### Terminal Coverage Report
```bash
pytest --cov=. --cov-report=term-missing
```

### Coverage Report for Specific Module
```bash
pytest --cov=parsers --cov-report=html tests/test_parsers.py
```

## Test Metrics

### Total Test Cases: 90+
- Unit Tests: 45+
- Integration Tests: 20+
- Service Tests: 15+
- API Tests: 25+

### Code Coverage Targets
- **Minimum Target**: 80% overall coverage
- **Critical Modules**: 90%+ (parsers, services)
- **API Routes**: 85%+
- **Config**: 100%

## Key Test Scenarios

### Parser Testing
✓ COBOL program parsing (metadata, divisions, variables, procedures)
✓ JCL job parsing (job cards, steps, datasets, utilities)
✓ Line counting and statistics
✓ Edge cases (empty files, minimal code, malformed input)

### Service Testing
✓ Ollama connection and availability
✓ Text generation and streaming
✓ Explanation generation with prompt building
✓ Complexity assessment and modernization suggestions
✓ Error handling and retries

### API Testing
✓ File upload and validation
✓ COBOL and JCL file analysis
✓ Health checks
✓ Parse-only endpoints
✓ Response validation
✓ Error responses

### Integration Testing
✓ Full analysis pipeline (parse → explain)
✓ Multiple file type support
✓ Concurrent uploads
✓ Data consistency
✓ Service integration

## Continuous Integration

For CI/CD pipelines, use:
```bash
pytest --cov=. --cov-report=xml --cov-report=term
```

This generates an XML report suitable for tools like:
- Jenkins
- GitHub Actions
- GitLab CI
- CodeCov

## Troubleshooting

### Async Test Issues
If async tests fail, ensure `pytest-asyncio` is installed:
```bash
pip install pytest-asyncio
```

### Import Errors
Ensure the backend directory is in PYTHONPATH:
```bash
export PYTHONPATH="${PYTHONPATH}:$(pwd)/backend"
pytest
```

### Mock Issues
Some tests use mocking for external services (Ollama). Ensure `unittest.mock` is available (built-in for Python 3.3+).

## Best Practices

1. **Test Independence**: Each test is independent and can run in any order
2. **Fixtures**: Reusable test data and mocks defined in conftest.py
3. **Mocking**: External services (Ollama) are mocked
4. **Documentation**: Each test has a docstring explaining what it tests
5. **Assertions**: Clear, specific assertions for easier debugging
6. **Names**: Descriptive test names that explain the scenario

## Adding New Tests

When adding new features:
1. Write tests first (TDD approach)
2. Add fixtures to conftest.py for reusability
3. Follow naming conventions: `test_<functionality>`
4. Include docstrings explaining the test scenario
5. Run coverage report to ensure adequate coverage
6. Update this README with new test categories

## Contact & Support

For issues with tests, refer to:
- [Pytest Documentation](https://docs.pytest.org/)
- [FastAPI Testing](https://fastapi.tiangolo.com/advanced/testing-dependencies/)
- Project documentation

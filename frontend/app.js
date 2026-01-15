const API_BASE = 'http://localhost:8000';

// DOM Elements
const healthBtn = document.getElementById('healthBtn');
const healthStatus = document.getElementById('healthStatus');
const fileInput = document.getElementById('fileInput');
const uploadBtn = document.getElementById('uploadBtn');
const fileInfo = document.getElementById('fileInfo');
const analyzeBtn = document.getElementById('analyzeBtn');
const progress = document.getElementById('progress');
const results = document.getElementById('results');
const error = document.getElementById('error');
const downloadBtn = document.getElementById('downloadBtn');
const newAnalysisBtn = document.getElementById('newAnalysisBtn');
const exportReportBtn = document.getElementById('exportReportBtn');

// Tab buttons
const tabBtns = document.querySelectorAll('.tab-btn');

// Global data storage
let currentAnalysisData = null;
let currentFile = null;

// Event Listeners
healthBtn.addEventListener('click', checkHealth);
uploadBtn.addEventListener('click', () => fileInput.click());
fileInput.addEventListener('change', handleFileSelect);
analyzeBtn.addEventListener('click', analyzeCode);
downloadBtn.addEventListener('click', downloadJSON);
newAnalysisBtn.addEventListener('click', resetApp);
exportReportBtn.addEventListener('click', exportFullReport);

// Tab switching
tabBtns.forEach(btn => {
    btn.addEventListener('click', () => {
        const tabName = btn.dataset.tab;
        switchTab(tabName);
    });
});

// Initialize
checkHealth();

// Functions
async function checkHealth() {
    try {
        const response = await fetch(`${API_BASE}/api/health`);
        const data = await response.json();
        
        displayHealthStatus(data);
    } catch (err) {
        displayHealthStatus({
            status: 'error',
            ollama_available: false,
            message: 'Cannot connect to backend server'
        });
    }
}

function displayHealthStatus(data) {
    healthStatus.classList.remove('hidden');
    
    if (data.ollama_available) {
        healthStatus.className = 'health-status';
        healthStatus.querySelector('.health-content').innerHTML = `
            <span style="font-size: 24px;">✅</span>
            <div>
                <strong>System Ready</strong>
                <p>Backend: ${data.status} | Ollama: Connected | Model: ${data.ollama_model}</p>
            </div>
        `;
    } else {
        healthStatus.className = 'health-status error';
        healthStatus.querySelector('.health-content').innerHTML = `
            <span style="font-size: 24px;">⚠️</span>
            <div>
                <strong>Ollama Not Running</strong>
                <p>Please start Ollama with: <code>ollama serve</code></p>
            </div>
        `;
    }
}

function handleFileSelect(e) {
    const file = e.target.files[0];
    if (!file) return;
    
    currentFile = file;
    
    // Display file info
    fileInfo.classList.remove('hidden');
    fileInfo.querySelector('.file-name').textContent = file.name;
    fileInfo.querySelector('.file-size').textContent = formatFileSize(file.size);
    
    // Enable analyze button
    analyzeBtn.disabled = false;
}

async function analyzeCode() {
    if (!currentFile) {
        showError('Please select a file first');
        return;
    }
    
    // Hide previous results/errors
    results.classList.add('hidden');
    error.classList.add('hidden');
    
    // Show progress
    progress.classList.remove('hidden');
    analyzeBtn.disabled = true;
    
    const formData = new FormData();
    formData.append('file', currentFile);
    
    try {
        const response = await fetch(`${API_BASE}/api/analyze`, {
            method: 'POST',
            body: formData
        });
        
        if (!response.ok) {
            const errorData = await response.json();
            throw new Error(errorData.detail || 'Analysis failed');
        }
        
        const data = await response.json();
        currentAnalysisData = data;
        
        // Hide progress, show results
        progress.classList.add('hidden');
        displayResults(data);
        
    } catch (err) {
        progress.classList.add('hidden');
        showError(err.message || 'Connection error. Is the server running?');
        console.error(err);
    } finally {
        analyzeBtn.disabled = false;
    }
}

function displayResults(data) {
    results.classList.remove('hidden');
    
    // Summary
    document.getElementById('fileName').textContent = data.filename;
    document.getElementById('codeType').textContent = data.code_type.toUpperCase();
    
    const complexityEl = document.getElementById('complexity');
    complexityEl.textContent = data.explanation.complexity_assessment;
    complexityEl.className = `value complexity ${data.explanation.complexity_assessment}`;
    
    document.querySelector('.processing-time').textContent = `${data.processing_time}s`;
    
    // Statistics
    displayStatistics(data.parsed_data.statistics);
    
    // Explanation
    document.getElementById('expSummary').textContent = data.explanation.summary;
    document.getElementById('expBusinessLogic').textContent = data.explanation.business_logic;
    document.getElementById('expTechnicalDetails').textContent = data.explanation.technical_details;
    
    const modernizationList = document.getElementById('expModernization');
    modernizationList.innerHTML = '';
    data.explanation.modernization_suggestions.forEach(suggestion => {
        const li = document.createElement('li');
        li.textContent = suggestion;
        modernizationList.appendChild(li);
    });
    
    // Parsed Structure
    displayParsedStructure(data.parsed_data);
    
    // Scroll to results
    results.scrollIntoView({ behavior: 'smooth' });
}

function displayStatistics(stats) {
    const statsContainer = document.getElementById('statistics');
    statsContainer.innerHTML = '';
    
    for (const [key, value] of Object.entries(stats)) {
        if (value !== null && value !== undefined) {
            const statDiv = document.createElement('div');
            statDiv.className = 'stat-item';
            statDiv.innerHTML = `
                <span class="stat-label">${formatLabel(key)}</span>
                <span class="stat-value">${value}</span>
            `;
            statsContainer.appendChild(statDiv);
        }
    }
}

function displayParsedStructure(parsedData) {
    // Metadata tab
    const metadataTab = document.getElementById('tab-metadata');
    if (parsedData.metadata) {
        metadataTab.innerHTML = createTable(parsedData.metadata);
    } else {
        metadataTab.innerHTML = '<p>No metadata available</p>';
    }
    
    // Variables tab
    const variablesTab = document.getElementById('tab-variables');
    if (parsedData.variables && parsedData.variables.length > 0) {
        variablesTab.innerHTML = createArrayTable(parsedData.variables);
    } else {
        variablesTab.innerHTML = '<p>No variables found</p>';
    }
    
    // Procedures tab  
    const proceduresTab = document.getElementById('tab-procedures');
    if (parsedData.procedures && parsedData.procedures.length > 0) {
        proceduresTab.innerHTML = createArrayTable(parsedData.procedures);
    } else if (parsedData.steps && parsedData.steps.length > 0) {
        proceduresTab.innerHTML = createArrayTable(parsedData.steps);
    } else {
        proceduresTab.innerHTML = '<p>No procedures/steps found</p>';
    }
    
    // Raw JSON tab
    document.getElementById('rawJson').textContent = JSON.stringify(parsedData, null, 2);
}

function createTable(obj) {
    let html = '<table class="data-table"><tbody>';
    for (const [key, value] of Object.entries(obj)) {
        html += `
            <tr>
                <th>${formatLabel(key)}</th>
                <td>${value || 'N/A'}</td>
            </tr>
        `;
    }
    html += '</tbody></table>';
    return html;
}

function createArrayTable(arr) {
    if (arr.length === 0) return '<p>No data</p>';
    
    const keys = Object.keys(arr[0]);
    let html = '<table class="data-table"><thead><tr>';
    
    keys.forEach(key => {
        html += `<th>${formatLabel(key)}</th>`;
    });
    html += '</tr></thead><tbody>';
    
    arr.forEach(item => {
        html += '<tr>';
        keys.forEach(key => {
            html += `<td>${item[key] || 'N/A'}</td>`;
        });
        html += '</tr>';
    });
    
    html += '</tbody></table>';
    return html;
}

function switchTab(tabName) {
    // Update buttons
    tabBtns.forEach(btn => {
        if (btn.dataset.tab === tabName) {
            btn.classList.add('active');
        } else {
            btn.classList.remove('active');
        }
    });
    
    // Update panes
    document.querySelectorAll('.tab-pane').forEach(pane => {
        if (pane.id === `tab-${tabName}`) {
            pane.classList.add('active');
        } else {
            pane.classList.remove('active');
        }
    });
}

function downloadJSON() {
    if (!currentAnalysisData) return;
    
    const blob = new Blob([JSON.stringify(currentAnalysisData.parsed_data, null, 2)], 
        { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `parsed-${currentAnalysisData.filename}-${Date.now()}.json`;
    a.click();
    URL.revokeObjectURL(url);
}

function exportFullReport() {
    if (!currentAnalysisData) return;
    
    const report = {
        metadata: {
            filename: currentAnalysisData.filename,
            code_type: currentAnalysisData.code_type,
            analysis_date: new Date().toISOString(),
            processing_time: currentAnalysisData.processing_time
        },
        parsed_structure: currentAnalysisData.parsed_data,
        explanation: currentAnalysisData.explanation
    };
    
    const blob = new Blob([JSON.stringify(report, null, 2)], 
        { type: 'application/json' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `full-report-${currentAnalysisData.filename}-${Date.now()}.json`;
    a.click();
    URL.revokeObjectURL(url);
}

function showError(message) {
    error.classList.remove('hidden');
    error.querySelector('.error-message').textContent = message;
    error.scrollIntoView({ behavior: 'smooth' });
}

function resetApp() {
    // Reset file input
    fileInput.value = '';
    currentFile = null;
    currentAnalysisData = null;
    
    // Hide sections
    fileInfo.classList.add('hidden');
    results.classList.add('hidden');
    error.classList.add('hidden');
    progress.classList.add('hidden');
    
    // Disable analyze button
    analyzeBtn.disabled = true;
    
    // Scroll to top
    window.scrollTo({ top: 0, behavior: 'smooth' });
}

// Utility Functions
function formatLabel(key) {
    return key.split('_')
        .map(word => word.charAt(0).toUpperCase() + word.slice(1))
        .join(' ');
}

function formatFileSize(bytes) {
    if (bytes === 0) return '0 Bytes';
    const k = 1024;
    const sizes = ['Bytes', 'KB', 'MB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return Math.round(bytes / Math.pow(k, i) * 100) / 100 + ' ' + sizes[i];
}
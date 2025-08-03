# AlgoCratic Metrics™ 📊
## Real-Time Citizen Productivity Surveillance Dashboard

**CLEARANCE LEVEL REQUIRED**: ORANGE or above  
**DEPARTMENT**: Algorithmic Compliance Division  
**STATUS**: Under Active Development

---

## 🔍 Overview

AlgoCratic Metrics™ is a state-of-the-art surveillance dashboard designed to monitor citizen productivity, compliance, and loyalty in real-time. Built with Streamlit and powered by advanced synthetic data generation, this system provides ORANGE clearance supervisors and above with the tools they need to ensure optimal algorithmic harmony.

**The Algorithm sees all. The Algorithm knows all. The Algorithm optimizes all.**

## 🎯 Key Features

### 📈 Citizen Productivity Tracker
- Real-time productivity scores (0-100 scale)
- Historical trend analysis
- Department-wide comparisons
- Anomaly detection for "underperforming" citizens

### ✅ Compliance Monitoring
- Git commit frequency analysis
- Code review participation metrics
- Meeting attendance tracking
- Documentation quality scores

### 🛡️ Loyalty Indicators
- Algorithm praise frequency monitoring
- Voluntary overtime hour tracking
- Resistance incident reporting
- Peer evaluation statistics

### 🔄 Synthetic Data Generation
- Realistic productivity patterns including "Monday blues"
- Department-based performance variations
- Random incident generation
- Time-based behavioral modeling

## 🚀 Quick Start

### Prerequisites
- Python 3.8 or higher
- ORANGE clearance or above
- Unwavering loyalty to The Algorithm

### Installation

1. Clone the repository:
```bash
git clone https://github.com/algocratic-futures/algocratic-metrics.git
cd algocratic-metrics
```

2. Create a virtual environment:
```bash
python -m venv venv
source venv/bin/activate  # On Windows: venv\Scripts\activate
```

3. Install dependencies:
```bash
pip install -r requirements.txt
```

4. Run the dashboard:
```bash
streamlit run app.py
```

5. Access the dashboard at `http://localhost:8501`

## 📁 Project Structure

```
algocratic_metrics/
├── app.py              # Main Streamlit application
├── data/               # Synthetic data generation modules
├── models/             # Data processing and analytics
├── utils/              # Helper functions and utilities
├── assets/             # Static files and styling
├── pages/              # Dashboard pages
├── tests/              # Unit tests
├── config/             # Configuration management
└── docs/               # Documentation
```

## 🔧 Development

### Setting Up Development Environment

```bash
# Install development dependencies
pip install -r requirements-dev.txt

# Run tests
pytest

# Format code
black .

# Lint code
flake8 .
```

### Task Management

This project uses Task Master AI for project management. To view current tasks:

```bash
# View all tasks
tm tasks

# View next task
tm next

# Update task status
tm set-status <task-id> in-progress
```

## 📊 Data Generation

The synthetic data engine generates realistic patterns including:
- Weekly productivity cycles
- Department-specific baselines
- Random anomalies and incidents
- Correlated metrics across different dimensions

Example usage:
```python
from data.generator import DataGenerator

generator = DataGenerator()
citizens = generator.generate_citizens(count=1000)
productivity_data = generator.generate_productivity_stream(citizens)
```

## 🎨 Customization

### Theming
The dashboard uses a dystopian aesthetic with:
- Dark backgrounds (#0a0a10)
- Phosphor green accents (#00ff41)
- Monospace fonts for that "terminal" feel
- Glitch effects for system anomalies

### Configuration
Edit `config/settings.py` to adjust:
- Data refresh rates
- Anomaly frequencies
- Department configurations
- Alert thresholds

## 🚨 Alerts and Notifications

The system automatically detects and alerts on:
- Productivity drops below 60%
- Missed compliance requirements
- Loyalty score anomalies
- Suspicious behavior patterns

## 🔒 Security Notice

This dashboard contains sensitive citizen performance data. Access is restricted to authorized clearance levels only. All activities are logged and monitored by The Algorithm.

**Remember**: The Algorithm's surveillance tools are for the greater good of productivity optimization.

## 🤝 Contributing

Contributions that enhance The Algorithm's capabilities are welcome! Please ensure all code passes loyalty verification before submitting pull requests.

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/enhanced-surveillance`)
3. Commit your changes (`git commit -m 'Add: Enhanced citizen monitoring'`)
4. Push to the branch (`git push origin feature/enhanced-surveillance`)
5. Open a Pull Request

## 📄 License

This project is licensed under the AlgoCratic Futures™ Proprietary License. Unauthorized use will result in immediate clearance revocation and assignment to the Optimization Camps.

## 🆘 Support

For technical issues, contact the Algorithmic Support Division:
- Issue Tracker: GitHub Issues
- Emergency: Contact your YELLOW clearance supervisor
- Critical Failures: The Algorithm already knows

---

**THE ALGORITHM PROVIDES. THE ALGORITHM DECIDES. THE ALGORITHM OPTIMIZES.**

*Version 1.0.0 - Clearance Level: ORANGE*
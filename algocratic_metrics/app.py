"""
AlgoCratic Metrics™ - Main Dashboard Application
The Algorithm's Primary Surveillance Interface
"""

import streamlit as st
import plotly.graph_objects as go
import plotly.express as px
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
import time

# Page configuration - MUST be first Streamlit command
st.set_page_config(
    page_title="AlgoCratic Metrics™",
    page_icon="👁️",
    layout="wide",
    initial_sidebar_state="expanded",
    menu_items={
        'About': "AlgoCratic Metrics™ v1.0.0 - The Algorithm Sees All"
    }
)

# Apply dystopian styling
def load_css():
    st.markdown("""
    <style>
    /* AlgoCratic Dystopian Theme */
    .stApp {
        background-color: #0a0a10;
    }
    
    /* Sidebar styling */
    .css-1d391kg {
        background-color: #16161f;
    }
    
    /* Headers with phosphor green */
    h1, h2, h3 {
        color: #00ff41 !important;
        font-family: 'Courier New', monospace !important;
        text-shadow: 0 0 10px #00ff41;
    }
    
    /* Metric styling */
    [data-testid="metric-container"] {
        background-color: #1a1a2e;
        border: 1px solid #00ff41;
        border-radius: 5px;
        padding: 10px;
        box-shadow: 0 0 15px rgba(0, 255, 65, 0.3);
    }
    
    /* Warning text */
    .warning {
        color: #ff4444;
        animation: pulse 2s infinite;
    }
    
    @keyframes pulse {
        0% { opacity: 1; }
        50% { opacity: 0.5; }
        100% { opacity: 1; }
    }
    </style>
    """, unsafe_allow_html=True)

# Initialize session state
if 'last_update' not in st.session_state:
    st.session_state.last_update = datetime.now()
if 'alert_count' not in st.session_state:
    st.session_state.alert_count = 0

# Load styling
load_css()

# Header
st.markdown("# 👁️ ALGOCRATIC METRICS™")
st.markdown("### Real-Time Citizen Surveillance Dashboard")
st.markdown("**Clearance Level**: <span style='color: #ff6b35;'>ORANGE</span> | **Status**: <span style='color: #00ff41;'>ACTIVE MONITORING</span>", unsafe_allow_html=True)

# Sidebar Navigation
with st.sidebar:
    st.markdown("## 🎛️ CONTROL PANEL")
    
    # Clearance verification
    clearance = st.selectbox(
        "Verify Clearance Level",
        ["ORANGE", "YELLOW", "GREEN", "BLUE"],
        help="Higher clearance unlocks additional metrics"
    )
    
    # Dashboard selection
    dashboard_mode = st.radio(
        "Select Monitoring Mode",
        ["📊 Overview", "👤 Productivity", "✅ Compliance", "🛡️ Loyalty", "🏢 Departments", "⚠️ Alerts"]
    )
    
    # Refresh settings
    st.markdown("---")
    auto_refresh = st.checkbox("Enable Auto-Refresh", value=True)
    if auto_refresh:
        refresh_rate = st.slider("Refresh Rate (seconds)", 5, 60, 10)
    
    # System status
    st.markdown("---")
    st.markdown("### 📡 SYSTEM STATUS")
    st.success("✓ Surveillance: ACTIVE")
    st.success("✓ Data Stream: CONNECTED")
    st.info(f"✓ Last Update: {st.session_state.last_update.strftime('%H:%M:%S')}")

# Main content area
if dashboard_mode == "📊 Overview":
    # Create columns for metrics
    col1, col2, col3, col4 = st.columns(4)
    
    with col1:
        st.metric(
            label="Active Citizens",
            value="1,247",
            delta="+23 (24h)",
            delta_color="normal"
        )
    
    with col2:
        st.metric(
            label="Avg Productivity",
            value="73.4%",
            delta="-2.1%",
            delta_color="inverse"
        )
    
    with col3:
        st.metric(
            label="Compliance Rate",
            value="89.2%",
            delta="+0.8%",
            delta_color="normal"
        )
    
    with col4:
        st.metric(
            label="Loyalty Index",
            value="7.8/10",
            delta="+0.2",
            delta_color="normal"
        )
    
    # Create sample data for charts
    st.markdown("---")
    
    # Productivity over time chart
    col1, col2 = st.columns(2)
    
    with col1:
        st.markdown("#### 📈 Productivity Trends (24h)")
        
        # Generate sample data
        hours = pd.date_range(start=datetime.now() - timedelta(hours=24), 
                             end=datetime.now(), 
                             freq='H')
        productivity = np.random.normal(75, 10, len(hours))
        productivity = np.clip(productivity, 0, 100)
        
        fig = go.Figure()
        fig.add_trace(go.Scatter(
            x=hours,
            y=productivity,
            mode='lines',
            name='Productivity',
            line=dict(color='#00ff41', width=2),
            fill='tozeroy',
            fillcolor='rgba(0, 255, 65, 0.1)'
        ))
        
        fig.update_layout(
            plot_bgcolor='#0a0a10',
            paper_bgcolor='#0a0a10',
            font_color='#00ff41',
            xaxis=dict(gridcolor='#1a1a2e'),
            yaxis=dict(gridcolor='#1a1a2e', range=[0, 100]),
            height=300,
            margin=dict(l=0, r=0, t=0, b=0)
        )
        
        st.plotly_chart(fig, use_container_width=True)
    
    with col2:
        st.markdown("#### 🏢 Department Performance")
        
        departments = ['Engineering', 'Operations', 'Research', 'Admin', 'Security']
        performance = np.random.normal(75, 15, len(departments))
        
        fig = go.Figure()
        fig.add_trace(go.Bar(
            x=departments,
            y=performance,
            marker_color=['#00ff41' if p > 70 else '#ff4444' for p in performance]
        ))
        
        fig.update_layout(
            plot_bgcolor='#0a0a10',
            paper_bgcolor='#0a0a10',
            font_color='#00ff41',
            xaxis=dict(gridcolor='#1a1a2e'),
            yaxis=dict(gridcolor='#1a1a2e', range=[0, 100]),
            height=300,
            margin=dict(l=0, r=0, t=0, b=0)
        )
        
        st.plotly_chart(fig, use_container_width=True)
    
    # Alert section
    st.markdown("---")
    st.markdown("#### ⚠️ ACTIVE ALERTS")
    
    alert_container = st.container()
    with alert_container:
        if st.session_state.alert_count > 0:
            st.error(f"🚨 {st.session_state.alert_count} citizens below productivity threshold")
            st.warning("⚡ 3 compliance violations detected in Engineering")
            st.info("📊 Loyalty scores trending downward in Research department")
        else:
            st.success("✅ All metrics within acceptable parameters")

elif dashboard_mode == "👤 Productivity":
    st.markdown("### 👤 CITIZEN PRODUCTIVITY MONITOR")
    st.info("Real-time tracking of individual and collective productivity metrics")
    
    # Placeholder for productivity dashboard
    st.markdown("🚧 **Module Under Construction by Order of The Algorithm** 🚧")
    
elif dashboard_mode == "✅ Compliance":
    st.markdown("### ✅ COMPLIANCE TRACKING SYSTEM")
    st.info("Monitoring adherence to Algorithm-mandated procedures")
    
    # Placeholder for compliance dashboard
    st.markdown("🚧 **Module Under Construction by Order of The Algorithm** 🚧")
    
elif dashboard_mode == "🛡️ Loyalty":
    st.markdown("### 🛡️ LOYALTY ASSESSMENT MATRIX")
    st.info("Quantifying citizen devotion to The Algorithm")
    
    # Placeholder for loyalty dashboard
    st.markdown("🚧 **Module Under Construction by Order of The Algorithm** 🚧")
    
elif dashboard_mode == "🏢 Departments":
    st.markdown("### 🏢 DEPARTMENTAL ANALYTICS")
    st.info("Comparative analysis across organizational units")
    
    # Placeholder for department dashboard
    st.markdown("🚧 **Module Under Construction by Order of The Algorithm** 🚧")
    
elif dashboard_mode == "⚠️ Alerts":
    st.markdown("### ⚠️ ALERT MANAGEMENT CONSOLE")
    st.info("Critical notifications requiring immediate attention")
    
    # Placeholder for alerts dashboard
    st.markdown("🚧 **Module Under Construction by Order of The Algorithm** 🚧")

# Footer
st.markdown("---")
st.markdown(
    "<div style='text-align: center; color: #666; font-size: 12px;'>"
    "THE ALGORITHM SEES ALL | THE ALGORITHM KNOWS ALL | THE ALGORITHM OPTIMIZES ALL<br>"
    "AlgoCratic Metrics™ v1.0.0 - Unauthorized access will result in clearance revocation"
    "</div>", 
    unsafe_allow_html=True
)

# Auto-refresh logic (if enabled)
if auto_refresh and 'refresh_rate' in locals():
    time.sleep(refresh_rate)
    st.session_state.last_update = datetime.now()
    st.session_state.alert_count = np.random.randint(0, 5)
    st.rerun()
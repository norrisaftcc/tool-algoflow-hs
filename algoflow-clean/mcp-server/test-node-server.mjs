#!/usr/bin/env node
// Quick test harness for the AlgoFlow Node MCP server

import { spawn } from 'child_process';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';

const __dirname = dirname(fileURLToPath(import.meta.url));
const serverPath = join(__dirname, 'algoflow-node.mjs');

// Spawn the server
const server = spawn('node', [serverPath], {
  stdio: ['pipe', 'pipe', 'pipe'] // Capture stderr to handle it properly
});

// Helper to send JSON-RPC message
function sendMessage(message) {
  const json = JSON.stringify(message);
  const length = Buffer.byteLength(json, 'utf8');
  server.stdin.write(`Content-Length: ${length}\r\n\r\n${json}`);
}

// Helper to parse responses
let buffer = '';
server.stdout.on('data', (chunk) => {
  buffer += chunk.toString();
  
  while (true) {
    const headerEnd = buffer.indexOf('\r\n\r\n');
    if (headerEnd === -1) break;
    
    const header = buffer.slice(0, headerEnd);
    const lengthMatch = header.match(/Content-Length: (\d+)/);
    if (!lengthMatch) {
      console.error('Invalid header:', header);
      buffer = buffer.slice(1); // Skip invalid data
      continue;
    }
    
    const length = parseInt(lengthMatch[1]);
    const messageStart = headerEnd + 4;
    
    if (buffer.length < messageStart + length) break;
    
    const message = buffer.slice(messageStart, messageStart + length);
    buffer = buffer.slice(messageStart + length);
    
    try {
      const response = JSON.parse(message);
      console.log('\n📨 Response:', JSON.stringify(response, null, 2));
    } catch (e) {
      console.error('Parse error:', e);
      console.error('Failed to parse:', message);
      console.error('Message length:', message.length, 'Expected:', length);
    }
  }
});

// Handle stderr separately
server.stderr.on('data', (chunk) => {
  console.error(chunk.toString().trim());
});

// Test sequence
async function runTests() {
  console.log('🧪 Testing AlgoFlow Node MCP Server...\n');
  
  // Wait for server to start
  await new Promise(resolve => setTimeout(resolve, 100));
  
  // 1. Initialize
  console.log('1️⃣ Initializing...');
  sendMessage({
    jsonrpc: '2.0',
    method: 'initialize',
    params: {},
    id: 1
  });
  
  await new Promise(resolve => setTimeout(resolve, 500));
  
  // 2. List tools
  console.log('\n2️⃣ Listing tools...');
  sendMessage({
    jsonrpc: '2.0',
    method: 'tools/list',
    params: {},
    id: 2
  });
  
  await new Promise(resolve => setTimeout(resolve, 500));
  
  // 3. List workflows (should show predefined)
  console.log('\n3️⃣ Listing workflows...');
  sendMessage({
    jsonrpc: '2.0',
    method: 'tools/call',
    params: {
      name: 'list_workflows',
      arguments: {}
    },
    id: 3
  });
  
  await new Promise(resolve => setTimeout(resolve, 500));
  
  // 4. Execute predefined workflow
  console.log('\n4️⃣ Executing "double" workflow with input 21...');
  sendMessage({
    jsonrpc: '2.0',
    method: 'tools/call',
    params: {
      name: 'execute_workflow',
      arguments: {
        workflow_id: 'double',
        inputs: 21
      }
    },
    id: 4
  });
  
  await new Promise(resolve => setTimeout(resolve, 500));
  
  // 5. Create custom workflow
  console.log('\n5️⃣ Creating custom workflow...');
  sendMessage({
    jsonrpc: '2.0',
    method: 'tools/call',
    params: {
      name: 'create_workflow',
      arguments: {
        name: 'my-pipeline',
        steps: [
          { name: 'step1', type: 'transform' },
          { name: 'step2', type: 'enrich' },
          { name: 'step3', type: 'filter' }
        ]
      }
    },
    id: 5
  });
  
  await new Promise(resolve => setTimeout(resolve, 500));
  
  // 6. Execute custom workflow
  console.log('\n6️⃣ Executing custom workflow...');
  sendMessage({
    jsonrpc: '2.0',
    method: 'tools/call',
    params: {
      name: 'execute_workflow',
      arguments: {
        workflow_id: 'my-pipeline',
        inputs: { test: 'data', foo: 'bar' }
      }
    },
    id: 6
  });
  
  await new Promise(resolve => setTimeout(resolve, 500));
  
  // 7. Check execution status
  console.log('\n7️⃣ Checking execution status...');
  sendMessage({
    jsonrpc: '2.0',
    method: 'tools/call',
    params: {
      name: 'workflow_status',
      arguments: {
        execution_id: 'all'
      }
    },
    id: 7
  });
  
  await new Promise(resolve => setTimeout(resolve, 1000));
  
  console.log('\n✅ Tests complete! Shutting down...');
  server.kill();
  process.exit(0);
}

// Handle errors
server.on('error', (err) => {
  console.error('Server error:', err);
  process.exit(1);
});

// Run tests
runTests().catch(console.error);
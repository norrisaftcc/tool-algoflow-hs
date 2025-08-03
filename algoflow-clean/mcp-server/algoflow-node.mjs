#!/usr/bin/env node
import { stdin, stdout } from 'node:process';
import { readFileSync, writeFileSync, existsSync } from 'node:fs';

// Workflow state - just a JSON file
const STATE_FILE = 'workflow-state.json';
let state = {
  workflows: {},
  executions: {},
  counter: 0
};

// Load state
if (existsSync(STATE_FILE)) {
  try {
    state = JSON.parse(readFileSync(STATE_FILE, 'utf8'));
  } catch (e) {
    console.error('Failed to load state:', e);
  }
}

// Save state
function saveState() {
  writeFileSync(STATE_FILE, JSON.stringify(state, null, 2));
}

// Simple workflow engine
class WorkflowEngine {
  static execute(workflow, input) {
    const startTime = Date.now();
    let result = input;
    let trace = [];
    
    try {
      for (const step of workflow.steps) {
        trace.push(`Executing ${step.name} (${step.type})`);
        
        switch (step.type) {
          case 'transform':
            if (typeof result === 'object') {
              result = { ...result, transformed: true, step: step.name };
            } else {
              result = `${result} [transformed by ${step.name}]`;
            }
            break;
            
          case 'filter':
            if (typeof result === 'object') {
              // Remove null/undefined values
              const filtered = {};
              for (const [k, v] of Object.entries(result)) {
                if (v !== null && v !== undefined) filtered[k] = v;
              }
              result = filtered;
            }
            break;
            
          case 'enrich':
            if (typeof result === 'object') {
              result = { ...result, enriched: true, timestamp: new Date().toISOString() };
            } else {
              result = { original: result, enriched: true, timestamp: new Date().toISOString() };
            }
            break;
            
          case 'double':
            result = typeof result === 'number' ? result * 2 : result;
            break;
            
          case 'concatenate':
            result = `${result} + ${result}`;
            break;
            
          default:
            trace.push(`Unknown step type: ${step.type}`);
        }
      }
      
      return {
        status: 'success',
        result,
        executionTime: (Date.now() - startTime) / 1000,
        trace
      };
    } catch (err) {
      return {
        status: 'error',
        error: err.message,
        executionTime: (Date.now() - startTime) / 1000,
        trace
      };
    }
  }
}

// Predefined workflows
const PREDEFINED_WORKFLOWS = {
  simple: {
    name: 'simple',
    steps: [{ name: 'stringify', type: 'transform' }]
  },
  double: {
    name: 'double',
    steps: [{ name: 'double', type: 'double' }]
  },
  concatenate: {
    name: 'concatenate',
    steps: [{ name: 'concat', type: 'concatenate' }]
  },
  pipeline: {
    name: 'pipeline',
    steps: [
      { name: 'transform', type: 'transform' },
      { name: 'enrich', type: 'enrich' },
      { name: 'filter', type: 'filter' }
    ]
  }
};

// Read JSON-RPC from stdin
async function readMessage() {
  return new Promise((resolve) => {
    let buffer = '';
    stdin.on('data', (chunk) => {
      buffer += chunk.toString();
      const lines = buffer.split('\n');
      
      for (let i = 0; i < lines.length - 1; i++) {
        const line = lines[i].trim();
        if (line.startsWith('Content-Length:')) {
          const length = parseInt(line.split(':')[1].trim());
          const messageStart = buffer.indexOf('\r\n\r\n') + 4;
          
          if (buffer.length >= messageStart + length) {
            const message = buffer.slice(messageStart, messageStart + length);
            buffer = buffer.slice(messageStart + length);
            
            try {
              const json = JSON.parse(message);
              resolve(json);
            } catch (e) {
              console.error('Parse error:', e);
            }
          }
        }
      }
    });
  });
}

// Send JSON-RPC to stdout
function sendMessage(message) {
  const json = JSON.stringify(message);
  const length = Buffer.byteLength(json, 'utf8');
  stdout.write(`Content-Length: ${length}\r\n\r\n${json}`);
}

// Main message loop
async function main() {
  console.error('🚀 AlgoFlow Node MCP Server Started (zero deps)');
  
  while (true) {
    const message = await readMessage();
    
    if (message.method === 'initialize') {
      sendMessage({
        jsonrpc: '2.0',
        id: message.id,
        result: {
          protocolVersion: '2024-11-05',
          capabilities: {
            tools: {}
          },
          serverInfo: {
            name: 'algoflow-node',
            version: '0.1.0'
          }
        }
      });
    }
    
    else if (message.method === 'tools/list') {
      sendMessage({
        jsonrpc: '2.0',
        id: message.id,
        result: {
          tools: [
            {
              name: 'create_workflow',
              description: 'Create a new workflow from a specification',
              inputSchema: {
                type: 'object',
                properties: {
                  name: { type: 'string', description: 'Workflow name' },
                  steps: {
                    type: 'array',
                    description: 'Workflow steps',
                    items: {
                      type: 'object',
                      properties: {
                        name: { type: 'string' },
                        type: { 
                          type: 'string', 
                          enum: ['transform', 'filter', 'enrich', 'double', 'concatenate'] 
                        }
                      },
                      required: ['name', 'type']
                    }
                  }
                },
                required: ['name', 'steps']
              }
            },
            {
              name: 'execute_workflow',
              description: 'Execute a workflow with given inputs',
              inputSchema: {
                type: 'object',
                properties: {
                  workflow_id: { type: 'string', description: 'Workflow ID' },
                  inputs: { 
                    type: ['object', 'number', 'string'], 
                    description: 'Input data' 
                  }
                },
                required: ['workflow_id', 'inputs']
              }
            },
            {
              name: 'list_workflows',
              description: 'List all available workflows',
              inputSchema: {
                type: 'object',
                properties: {}
              }
            },
            {
              name: 'workflow_status',
              description: 'Get status of workflow executions',
              inputSchema: {
                type: 'object',
                properties: {
                  execution_id: { type: 'string', description: 'Execution ID or "all"' }
                }
              }
            }
          ]
        }
      });
    }
    
    else if (message.method === 'tools/call') {
      const { name, arguments: args } = message.params;
      let result = '';
      
      // CREATE WORKFLOW
      if (name === 'create_workflow') {
        const workflow = {
          name: args.name,
          steps: args.steps,
          created: new Date().toISOString()
        };
        
        state.workflows[args.name] = workflow;
        saveState();
        
        result = `✅ Workflow "${args.name}" created with ${args.steps.length} steps:\n`;
        args.steps.forEach((step, i) => {
          result += `  ${i + 1}. ${step.name} (${step.type})\n`;
        });
      }
      
      // EXECUTE WORKFLOW
      else if (name === 'execute_workflow') {
        const workflowId = args.workflow_id;
        const input = args.inputs;
        
        // Check predefined first
        const workflow = PREDEFINED_WORKFLOWS[workflowId] || state.workflows[workflowId];
        
        if (!workflow) {
          result = `❌ Workflow "${workflowId}" not found`;
        } else {
          const executionId = `exec_${++state.counter}`;
          const execution = WorkflowEngine.execute(workflow, input);
          
          // Store execution
          state.executions[executionId] = {
            id: executionId,
            workflowId,
            input,
            output: execution,
            timestamp: new Date().toISOString()
          };
          saveState();
          
          result = `🚀 Execution ${executionId} completed\n`;
          result += `Workflow: ${workflowId}\n`;
          result += `Status: ${execution.status}\n`;
          result += `Time: ${execution.executionTime}s\n`;
          
          if (execution.status === 'success') {
            result += `Result: ${JSON.stringify(execution.result, null, 2)}\n`;
          } else {
            result += `Error: ${execution.error}\n`;
          }
          
          if (execution.trace.length > 0) {
            result += '\nTrace:\n';
            execution.trace.forEach(t => result += `  • ${t}\n`);
          }
        }
      }
      
      // LIST WORKFLOWS
      else if (name === 'list_workflows') {
        const custom = Object.keys(state.workflows);
        const predefined = Object.keys(PREDEFINED_WORKFLOWS);
        
        result = '=== Available Workflows ===\n\n';
        
        if (predefined.length > 0) {
          result += 'Predefined:\n';
          predefined.forEach(w => {
            const wf = PREDEFINED_WORKFLOWS[w];
            result += `  • ${w}: ${wf.steps.length} steps\n`;
          });
        }
        
        if (custom.length > 0) {
          result += '\nCustom:\n';
          custom.forEach(w => {
            const wf = state.workflows[w];
            result += `  • ${w}: ${wf.steps.length} steps (created ${wf.created})\n`;
          });
        }
        
        if (predefined.length === 0 && custom.length === 0) {
          result = 'No workflows available';
        }
      }
      
      // WORKFLOW STATUS
      else if (name === 'workflow_status') {
        const id = args.execution_id;
        
        if (id === 'all') {
          const executions = Object.values(state.executions);
          result = `=== Workflow Executions (${executions.length} total) ===\n\n`;
          
          executions.slice(-10).forEach(exec => {
            result += `${exec.id}: ${exec.workflowId} - ${exec.output.status} (${exec.timestamp})\n`;
          });
          
          if (executions.length > 10) {
            result += `\n... and ${executions.length - 10} more`;
          }
        } else if (state.executions[id]) {
          const exec = state.executions[id];
          result = `=== Execution ${id} ===\n`;
          result += `Workflow: ${exec.workflowId}\n`;
          result += `Time: ${exec.timestamp}\n`;
          result += `Input: ${JSON.stringify(exec.input)}\n`;
          result += `Status: ${exec.output.status}\n`;
          
          if (exec.output.result) {
            result += `Output: ${JSON.stringify(exec.output.result, null, 2)}\n`;
          }
          if (exec.output.error) {
            result += `Error: ${exec.output.error}\n`;
          }
        } else {
          result = `❌ Execution ${id} not found`;
        }
      }
      
      // Send response
      sendMessage({
        jsonrpc: '2.0',
        id: message.id,
        result: {
          content: [
            {
              type: 'text',
              text: result
            }
          ]
        }
      });
    }
  }
}

// Handle errors
process.on('uncaughtException', (err) => {
  console.error('AlgoFlow error:', err);
});

// Start
main().catch(console.error);
# Wave Function Autonomous Operation Guide

## Overview

The Wave Function Autonomous Operation System provides a comprehensive framework for building autonomous, self-improving AI systems using Emacs Lisp. The system implements three levels of autonomy with corresponding Y-combinator recursion patterns, async/await execution, and multi-protocol communication.

## Architecture

### Core Components

1. **Y-Combinator System** (`wave-y-combinator.el`)
   - Private Y-combinator: Internal self-improvement
   - Public Y-combinator: User-facing with supervision
   - Shared Y-combinator: Multi-agent collaboration

2. **Async Framework** (`wave-async-framework.el`)
   - Timer-based execution
   - Process-based execution
   - Hybrid execution
   - Try/catch error handling

3. **Workflow Engine** (`wave-workflow-engine.el`)
   - YAML-configurable workflows
   - {read, eval, print, loop} operations
   - Async execution support

4. **Incidence Workflow** (`wave-incidence-workflow.el`)
   - Graph-based workflow definitions
   - Vertices and edges
   - Incidence matrix validation

5. **Protocol Adapters** (`wave-protocol-adapter.el`)
   - Emacs client protocol
   - WebSocket protocol
   - MQTT protocol
   - Incidence relation tracking

6. **Autonomous REPL** (`wave-autonomous-repl.el`)
   - Three control levels
   - Visual buffer interface
   - Y-combinator recursion loop

7. **System Initialization** (`wave-autonomous-init.el`)
   - Full system startup
   - Quick start menu
   - Health monitoring

## Control Levels

### 1. Full Supervision (Public Y-combinator)
- **User Approval**: Required for every action
- **Safety Constraints**: Maximum depth, timeout, user supervision
- **Use Case**: Critical operations, learning phase
- **Y-Combinator Level**: Public

```elisp
(wave-autonomous-repl-start "full-supervision")
```

### 2. Guided Autonomy (Shared Y-combinator)
- **Boundary Constraints**: Geometric and incidence validation
- **Collaborative**: Multi-agent coordination
- **Use Case**: Production systems, collaborative AI
- **Y-Combinator Level**: Shared

```elisp
(wave-autonomous-repl-start "guided-autonomy")
```

### 3. Full Autonomy (Private Y-combinator)
- **Minimal Intervention**: Self-improvement and evolution
- **Internal Operations**: Autonomous learning and adaptation
- **Use Case**: Research, self-evolving systems
- **Y-Combinator Level**: Private

```elisp
(wave-autonomous-repl-start "full-autonomy")
```

## Y-Combinator Theory

### Fixed-Point Recursion

The Y-combinator enables fixed-point recursion in functional programming:

```elisp
(defun wave-y-combinator-private (f)
  "Private Y-combinator for internal autonomous operations"
  ((lambda (x) (funcall x x))
   (lambda (x) (funcall f (lambda (&rest args) 
                            (apply (funcall x x) args))))))
```

### Three Levels of Control

1. **Private**: Unrestricted recursion for self-improvement
2. **Public**: User-supervised recursion with approval hooks
3. **Shared**: Collaborative recursion with geometric constraints

## Async/Await Framework

### Hybrid Execution

The system supports three execution modes:

1. **Timer-based**: Simple periodic checks, works in batch mode
2. **Process-based**: Separate Emacs processes, full parallelism
3. **Hybrid**: Timers for UI, processes for heavy computation

### Try/Catch Error Handling

```elisp
(defun wave-async-try-catch (try-operation catch-operation)
  "Try operation with catch for error handling"
  (let ((operation (wave-async-operation-create
                    "try-catch"
                    try-operation
                    'timer)))
    (wave-async-operation-set-callbacks
     operation
     nil  ; success callback
     (lambda (error op)
       (when catch-operation
         (funcall catch-operation error op))))
    (wave-async-execute operation)
    operation))
```

## Workflow Definition

### YAML Configuration

Workflows are defined in YAML with the following structure:

```yaml
workflow:
  id: "autonomous-repl-example"
  type: "repl"
  y_combinator_level: "guided-autonomy"
  
  read:
    source: "user-input"
    input_type: "s-expression"
    protocol: "emacsclient"
    
  eval:
    transform: "autonomous-process-input"
    async:
      enabled: true
      executor: "hybrid"
      timeout: 5.0
      
  print:
    output: "buffer"
    target: "*Autonomous-REPL*"
    format: "pretty-print"
    
  loop:
    condition: "(lambda (ctx) (not (plist-get ctx :quit)))"
    
  error:
    try: "eval-transform"
    catch: "(lambda (err) (message \"Error: %s\" err))"
    await: "user-decision"
    recovery: "fallback-to-simple"
```

### Incidence-Based Workflows

Graph-based workflows using vertices, edges, and incidence matrices:

```yaml
incidence_workflow:
  id: "autonomous-learning-workflow"
  
  vertices:
    - id: "v0-read-input"
      operation: "read-user-input"
      type: "input"
      
    - id: "v1-analyze"
      operation: "autonomous-analyze-pattern"
      type: "eval"
      
  edges:
    - id: "e0"
      from: "v0-read-input"
      to: "v1-analyze"
      condition: "input-valid"
      
  incidence_matrix:
    rows: ["v0", "v1"]
    cols: ["e0"]
    data: [[1], [0]]
```

## Protocol Adapters

### Multi-Protocol Support

The system supports three communication protocols:

1. **Emacs Client**: External connections to Emacs server
2. **WebSocket**: Browser/external connections via HTTP
3. **MQTT**: IoT/message-based communication

### Incidence Relation Tracking

All protocols track geometric communication patterns:

```elisp
(defun wave-protocol-track-incidence-relation (adapter message)
  "Track incidence relations for geometric communication"
  (let ((incidence-data (wave-protocol-message-incidence-data message))
        (geometric-metadata (wave-protocol-message-geometric-metadata message)))
    (when incidence-data
      (puthash (wave-protocol-message-message-id message) incidence-data
               (wave-protocol-adapter-incidence-relations adapter)))))
```

## REPL Interface

### Visual Buffer Interface

The autonomous REPL provides a rich visual interface with:

- Real-time status updates
- Control level indicators
- Approval request handling
- Execution history
- Error reporting

### Keybindings

- `C-c C-i`: Insert autonomous input
- `C-c C-e`: Execute with current control level
- `C-c C-l`: Change control level
- `C-c C-a`: Approve autonomous action
- `C-c C-d`: Deny autonomous action
- `C-c C-s`: Show autonomous state
- `C-c C-h`: View workflow history
- `C-c C-p`: Pause REPL
- `C-c C-r`: Resume REPL
- `C-c C-q`: Quit REPL

## System Initialization

### Full System Startup

```elisp
(wave-autonomous-system-init)
```

This initializes:
- Protocol adapters (emacsclient, websocket, mqtt)
- Workflow definitions from YAML files
- Autonomous evolution engine
- Default REPL with guided autonomy
- System monitoring and health checks

### Quick Start Menu

```elisp
(wave-autonomous-quick-start)
```

Provides options for:
- Full Supervision (public Y-combinator)
- Guided Autonomy (shared Y-combinator)
- Full Autonomy (private Y-combinator)
- Custom Workflow from YAML
- System Status & Diagnostics

## Safety and Constraints

### Safety Constraints

Each Y-combinator level has specific safety constraints:

1. **Private**: Maximum depth, timeout
2. **Public**: Maximum depth, timeout, user supervision
3. **Shared**: Maximum depth, timeout, incidence validation, geometric validation

### Approval System

The approval system ensures user control over autonomous operations:

```elisp
(defun wave-repl-needs-approval (repl input)
  "Check if input needs user approval"
  (let ((control-level (wave-autonomous-repl-control-level repl)))
    (pcase control-level
      ('full-supervision t)
      ('guided-autonomy (wave-repl-check-guided-approval repl input))
      ('full-autonomy nil))))
```

## Testing

### Comprehensive Test Suite

The system includes a comprehensive test suite (`test-autonomous-operation.el`):

```elisp
(test-autonomous-operation-all)
```

Tests include:
- Y-combinator levels (private, public, shared)
- Async/await/try/catch workflow
- REPL modes (full-supervision, guided-autonomy, full-autonomy)
- YAML workflow loading and execution
- Protocol adapters (emacsclient, websocket, mqtt)
- Incidence-based workflows
- System integration tests

### Quick Tests

```elisp
(test-autonomous-operation-quick)
```

Runs a subset of critical tests for rapid validation.

## Example Workflows

### 1. Autonomous Learning Workflow

```yaml
workflow:
  id: "autonomous-learning"
  type: "repl"
  y_combinator_level: "shared"
  
  read:
    source: "user-input"
    input_type: "s-expression"
    
  eval:
    transform: "autonomous-learn-from-pattern"
    async:
      enabled: true
      executor: "hybrid"
      
  print:
    output: "buffer"
    target: "*Learning-Output*"
    
  spo_modality:
    subject: "autonomous-agent"
    predicate: "learns-from"
    object: "user-input"
    modality: "supervised"
```

### 2. IoT Integration Workflow

```yaml
workflow:
  id: "mqtt-iot-integration"
  type: "repl"
  y_combinator_level: "shared"
  
  read:
    source: "mqtt-broker"
    input_type: "iot-message"
    protocol: "mqtt"
    
  eval:
    transform: "iot-data-processing"
    async:
      enabled: true
      executor: "hybrid"
      
  print:
    output: "mqtt-response"
    target: "iot-devices"
    format: "json"
    
  spo_modality:
    subject: "iot-gateway"
    predicate: "processes-data-from"
    object: "iot-devices"
    modality: "autonomous"
```

## Best Practices

### 1. Control Level Selection

- **Full Supervision**: Use for critical operations and learning phases
- **Guided Autonomy**: Use for production systems and collaborative AI
- **Full Autonomy**: Use for research and self-evolving systems

### 2. Workflow Design

- Keep workflows simple and focused
- Use appropriate async execution for long-running operations
- Implement proper error handling and recovery
- Define clear SPO modalities for semantic clarity

### 3. Safety Considerations

- Always test workflows in full supervision mode first
- Implement proper approval mechanisms for dangerous operations
- Use geometric constraints for shared operations
- Monitor system health regularly

### 4. Performance Optimization

- Use hybrid async execution for optimal performance
- Implement proper timeout handling
- Use incidence-based workflows for complex operations
- Monitor execution times and optimize bottlenecks

## Troubleshooting

### Common Issues

1. **Y-combinator recursion depth exceeded**
   - Increase maximum depth limits
   - Check for infinite recursion
   - Use appropriate control level

2. **Async operation timeout**
   - Increase timeout values
   - Check for blocking operations
   - Use process-based execution for heavy computation

3. **Protocol adapter connection failed**
   - Check server availability
   - Verify configuration
   - Test with minimal setup

4. **Workflow execution failed**
   - Validate YAML syntax
   - Check function definitions
   - Test individual components

### Debug Commands

```elisp
;; System status
(wave-autonomous-system-status)

;; Inspect specific components
(wave-autonomous-repl-inspect repl)
(wave-protocol-adapter-inspect adapter)
(wave-workflow-inspect workflow)

;; Health check
(wave-autonomous-system-health)
```

## Conclusion

The Wave Function Autonomous Operation System provides a powerful framework for building autonomous AI systems with multiple levels of control and safety. The combination of Y-combinator recursion, async execution, and multi-protocol communication enables sophisticated autonomous behaviors while maintaining user control and system safety.

The system is designed to be extensible and customizable, allowing developers to create their own workflows, protocols, and autonomous behaviors while leveraging the core framework for safety, monitoring, and coordination.

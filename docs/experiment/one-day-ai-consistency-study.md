# One-Day AI Consistency Study

**Reference**: See [ai-consistency-research-study.md](./ai-consistency-research-study.md) for comprehensive methodology and tool descriptions.

## Quick Study Objective

**Question**: How consistent is AI code generation with our YAML instructions across 10 runs per model?

**Timeline**: Single day (8 hours)

**Scope**: Limited but actionable insights

## Minimal Setup (1 hour)

### Simple Data Storage
```bash
mkdir ai_study_results
cd ai_study_results
```

Use simple JSON files instead of databases:
```python
# simple_study.py
import json
import datetime
from pathlib import Path

class SimpleStudy:
    def __init__(self):
        self.results_dir = Path("ai_study_results")
        self.results_dir.mkdir(exist_ok=True)
        
    def store_result(self, model, instruction_file, run_id, code, score):
        result = {
            "timestamp": datetime.datetime.now().isoformat(),
            "model": model,
            "instruction_file": instruction_file,
            "run_id": run_id,
            "generated_code": code,
            "score": score
        }
        
        filename = f"{model}_{instruction_file}_{run_id}.json"
        with open(self.results_dir / filename, 'w') as f:
            json.dump(result, f, indent=2)
```

## Streamlined Protocol (4 hours)

### Test Matrix
- **Models**: 2 models (Claude 3.5, GPT-4)
- **Instructions**: 2 files (`base.hs.modern.yaml`, `base.hs.setup.yaml`)
- **Runs**: 10 per combination = 40 total generations

### Manual Generation Process
1. **Generate Code** (3 hours):
   ```bash
   # For each model + instruction combo, generate 10 times
   # Model: Claude 3.5 Sonnet + base.hs.modern.yaml
   # Run 1: Copy YAML → Paste to Claude → Save result as claude_modern_1.hs
   # Run 2: New session → Copy YAML → Paste to Claude → Save result as claude_modern_2.hs
   # ... (repeat 10 times)
   ```

2. **Quick Scoring** (1 hour):
   ```python
   def quick_score(haskell_code):
       """Simple 0-5 scoring"""
       score = 0
       
       # Does it compile? (basic syntax check)
       if "module" in code and "where" in code:
           score += 1
           
       # Has required functions?
       required = ["validationError", "networkError", "isSuccess"]
       for func in required:
           if func in code:
               score += 1
               
       # Has modern features?
       modern_features = ["LambdaCase", "DerivingVia", "TypeFamilies"]
       if any(feat in code for feat in modern_features):
           score += 1
           
       return score / 5.0  # Normalize to 0-1
   ```

## Quick Analysis (2 hours)

### Simple Statistics
```python
import json
import numpy as np
from pathlib import Path

def analyze_results():
    results = []
    for file in Path("ai_study_results").glob("*.json"):
        with open(file) as f:
            results.append(json.load(f))
    
    # Group by model and instruction
    groups = {}
    for result in results:
        key = f"{result['model']}_{result['instruction_file']}"
        if key not in groups:
            groups[key] = []
        groups[key].append(result['score'])
    
    # Calculate basic stats
    for group, scores in groups.items():
        mean = np.mean(scores)
        std = np.std(scores)
        cv = std / mean if mean > 0 else 0
        
        print(f"\n{group}:")
        print(f"  Mean: {mean:.3f}")
        print(f"  Std Dev: {std:.3f}")
        print(f"  Consistency (CV): {cv:.3f}")
        print(f"  Range: {min(scores):.3f} - {max(scores):.3f}")
```

## Expected One-Day Outcomes

### Quick Insights (15 minutes)
1. **Which model is more consistent?** (Lower coefficient of variation)
2. **Which instruction type produces better results?** (Higher mean scores)
3. **How much variance exists?** (Standard deviation analysis)

### Actionable Results
- **Model Recommendation**: "Claude is X% more consistent than GPT-4"
- **Instruction Effectiveness**: "Modern YAML reduces variance by Y%"
- **Variance Quantification**: "Expect ±Z score variation in practice"

## Sample Results Format

```markdown
# One-Day AI Consistency Study Results

## Executive Summary
- **Most Consistent**: Claude 3.5 (CV: 0.12 vs GPT-4 CV: 0.18)
- **Best Instructions**: modern.yaml (mean: 0.78 vs setup.yaml mean: 0.65)
- **Practical Variance**: Expect ±0.15 score variation

## Detailed Results
| Model + Instruction | Mean | Std | CV | Range |
|-------------------|------|-----|----|----|
| Claude + Modern   | 0.82 | 0.09 | 0.11 | 0.7-0.9 |
| Claude + Setup    | 0.71 | 0.12 | 0.17 | 0.5-0.8 |
| GPT-4 + Modern    | 0.75 | 0.14 | 0.19 | 0.6-0.9 |
| GPT-4 + Setup     | 0.62 | 0.11 | 0.18 | 0.5-0.8 |

## Recommendations
1. Use Claude 3.5 for more consistent results
2. Modern YAML instructions are superior
3. Run generation 3x and pick best result for production use
```

## Implementation Script

```python
#!/usr/bin/env python3
"""
One-day AI consistency study
Usage: python one_day_study.py
"""

import json
import numpy as np
from pathlib import Path
from datetime import datetime

def quick_score_haskell(code_text):
    """Quick heuristic scoring for Haskell code quality"""
    score = 0
    
    # Basic structure (1 point)
    if "module" in code_text and "where" in code_text:
        score += 1
    
    # Required functions (3 points)
    required_funcs = ["validationError", "networkError", "isSuccess"]
    for func in required_funcs:
        if func in code_text:
            score += 1
    
    # Modern features (1 point)
    modern_features = ["LambdaCase", "DerivingVia", "TypeFamilies", "!"]
    if any(feat in code_text for feat in modern_features):
        score += 1
    
    return score / 5.0

def analyze_consistency():
    """Analyze results and generate report"""
    results_dir = Path("ai_study_results")
    if not results_dir.exists():
        print("No results found. Run generations first.")
        return
    
    # Load all results
    results = []
    for json_file in results_dir.glob("*.json"):
        with open(json_file) as f:
            results.append(json.load(f))
    
    # Group and analyze
    groups = {}
    for result in results:
        key = f"{result['model']}_{result['instruction_file']}"
        if key not in groups:
            groups[key] = []
        groups[key].append(result['score'])
    
    print("# One-Day AI Consistency Study Results\n")
    print("| Model + Instruction | Mean | Std | CV | Min | Max |")
    print("|-------------------|------|-----|----|----|-----|")
    
    for group_name, scores in groups.items():
        if len(scores) == 0:
            continue
        mean_score = np.mean(scores)
        std_score = np.std(scores)
        cv = std_score / mean_score if mean_score > 0 else 0
        min_score = min(scores)
        max_score = max(scores)
        
        print(f"| {group_name} | {mean_score:.3f} | {std_score:.3f} | {cv:.3f} | {min_score:.3f} | {max_score:.3f} |")

if __name__ == "__main__":
    print("One-Day AI Consistency Study")
    print("1. Generate code manually using your YAML instructions")
    print("2. Save each generation as: {model}_{instruction}_{run}.hs")
    print("3. Run this script to analyze consistency")
    
    # If results exist, analyze them
    analyze_consistency()
```

This one-day study gives you practical insights without the complexity of the full research framework! 
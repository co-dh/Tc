# Implementation Plan: 4 Financial Plot Types

## Files to modify

### 1. `Tc/Cmd.lean` — Add PlotKind variants
- Add `.area`, `.density`, `.step`, `.violin` to `PlotKind`
- Add toString cases for each new variant

### 2. `Tc/Plot.lean` — Core plot logic
- Add all 4 to `cyclableKinds` array (enables h/l cycling)
- Add `rScript` geom cases:
  - `.area` → `geom_area(alpha = 0.4)` + fill aesthetics
  - `.density` → `geom_density(fill = 'steelblue', alpha = 0.5)`
  - `.step` → `geom_step(linewidth = 0.5)`
  - `.violin` → `geom_violin()` (same boxplot data flow)
- Density uses histogram data path (single column, no x-axis group needed)
- Violin uses boxplot data path (category x + numeric y)
- Update `Plot.update` to map `.plot .up` → `.plot .area`
- Handle density in the histogram branch of `run` (no group col needed)

### 3. `Tc/Key.lean` — Keybinding
- Add `('a', "area — ...", .up)` to `verbsFor 'P'` array

### 4. `test/Test.lean` — Tests
- `test_plot_render_area`: line.csv data → area chart PNG
- `test_plot_render_density`: numeric data → density plot PNG
- `test_plot_render_step`: line.csv data → step chart PNG
- `test_plot_render_violin`: mixed.csv data → violin plot PNG

## Keybinding summary
| Type | Direct key | Via cycling |
|------|-----------|-------------|
| Area | `Pa` | h/l from any plot |
| Density | (none) | h/l from any plot |
| Step | (none) | h/l from any plot |
| Violin | (none) | h/l from any plot |

## Data flow
- Area, Step: identical to line (x-axis group + numeric y)
- Density: identical to histogram (single numeric column, no grouping)
- Violin: identical to boxplot (category x + numeric y)

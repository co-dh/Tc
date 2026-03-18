# Financial Plot Types - Implementation Summary

## Current State
5 plot types: line, scatter, bar, histogram, boxplot
All rendered via R/ggplot2, displayed via kitty/viu/xdg-open.
Interactive mode: h/l cycles plot types, ,/. adjusts downsampling.

## New Plot Types (Finance-focused)

### 1. Area Chart (`geom_area`)
- **Use**: Portfolio NAV, cumulative returns, AUM over time
- **Key**: `Pa` (mapped via Verb.up)
- **Data flow**: Same as line chart (x + y + optional category)
- **R**: `geom_area(alpha = 0.4)` with `scale_fill_viridis_d()`

### 2. Density Plot (`geom_density`)
- **Use**: Return distributions, risk/VaR, smoother than histogram
- **Data flow**: Same as histogram (single numeric column)
- **R**: `geom_density(fill = 'steelblue', alpha = 0.5)`

### 3. Step Chart (`geom_step`)
- **Use**: Interest rates, dividends, discrete regime changes
- **Data flow**: Same as line chart
- **R**: `geom_step(linewidth = 0.5)`

### 4. Violin Plot (`geom_violin`)
- **Use**: Return distributions by asset/sector (richer than boxplot)
- **Data flow**: Same as boxplot (category x, numeric y)
- **R**: `geom_violin()` with optional `geom_boxplot(width=0.1)`

## Architecture Notes
- PlotKind enum in Cmd.lean: add .area, .density, .step, .violin
- cyclableKinds in Plot.lean: add all 4 (accessible via h/l cycling)
- rScript in Plot.lean: add geom cases for each new type
- Plot.update: map Verb.up → .area; density/step/violin via cycling only
- Key.lean verbsFor 'P': add ('a', "area", .up) entry
- Info.lean: no change needed (hints are compact)
- Tests: add R render tests for area, density, step, violin

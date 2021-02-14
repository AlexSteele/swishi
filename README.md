
# swishi

_swiss haskell image tool_

swishi is a command line image processing tool.

Use swishi to apply transforms to image files.

# Usage

```
usage: swishi [option...] [transform...] <image path> <output path>
options:
  -l list transforms
  -f output format
  -h print help
```

# Examples

```
# Convert to black and white
swishi bw image.png result.png
```

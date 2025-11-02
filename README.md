# CLAD

A source-code based CAD design system in Common Lisp with OpenCASCADE Technology integration.

## Prerequisites

### System Dependencies

#### Ubuntu/Debian
```bash
# OpenCASCADE Technology libraries
sudo apt-get install libocct-foundation-dev \
                     libocct-modeling-data-dev \
                     libocct-modeling-algorithms-dev \
                     libocct-data-exchange-dev

# Common Lisp (SBCL recommended)
sudo apt-get install sbcl

# Quicklisp (if not already installed)
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval '(quicklisp-quickstart:install)' --quit
```

#### macOS
```bash
# Install via Homebrew
brew install opencascade sbcl
```

### Common Lisp Dependencies

The following will be automatically installed via Quicklisp:
- CFFI (Foreign Function Interface)
- Alexandria (Utilities)
- Trivial-Garbage (Finalization)
- FiveAM (Testing framework)

## Installation

1. Clone this repository:
```bash
git clone <repository-url>
cd clad
```

2. Load into your Lisp REPL:
```lisp
(load "clad.asd")
(ql:quickload :clad)
```

## Running Tests

```lisp
(asdf:test-system :clad)
```

## Development Status

Currently implementing **Phase 1: Foundation**

- [x] Project structure
- [ ] CFFI bindings with exception handling
- [ ] Functional core primitives
- [ ] Basic units system
- [ ] STEP export
- [ ] Comprehensive tests

## Quick Start (Coming Soon)

```lisp
;; Load the system
(ql:quickload :clad)

;; Create a simple part
(defpart simple-box ()
  (:body
    (box :width (dim 100)
         :height (dim 50)
         :depth (dim 25))))

;; Export to STEP
(export-step (simple-box) "output.step")
```

## Documentation

See [design_guide.md](design_guide.md) for the complete system architecture and implementation plan.

## License

MIT

# CLAUDE.md - polyglot-i18n

## Project Overview

**polyglot-i18n** is a ReScript-first, WASM-accelerated translation module with dynamic JSON storage. Fork of [i18n-node](https://github.com/mashpie/i18n-node) by Marcus Spiegel, reimagined for modern polyglot development.

**Key Features:**
- ReScript-first implementation (TypeScript eliminated)
- WASM-accelerated hot paths (Rust)
- Multi-runtime: Node.js, Deno, Bun
- Common `__('...')` syntax for translations
- Plural forms handling with CLDR rules
- MessageFormat support for advanced formatting
- Mustache template support
- sprintf-style formatting
- Object notation for hierarchical translations
- Guix-first, Nix fallback packaging
- Chainguard Wolfi containers

## Architecture

### Core Components

1. **i18n.js** - Legacy JavaScript implementation (being migrated to ReScript)
   - Translation engine
   - Locale management
   - File I/O for JSON catalogs
   - API methods for translations

2. **bindings/rescript/** - ReScript type definitions and bindings
   - Type-safe FFI to JavaScript core
   - Future: Pure ReScript implementation

3. **wasm/** - Rust WASM module (planned)
   - High-performance plural rules
   - MessageFormat parsing
   - Interpolation engine

4. **deno/** - Deno runtime module
   - TypeScript definitions
   - Deno Deploy compatible

5. **locales/** - Translation catalog storage (JSON files per locale)

### Dependencies

**Runtime:**
- `@messageformat/core` - MessageFormat ICU formatting
- `debug` - Logging infrastructure
- `fast-printf` - sprintf-style formatting
- `make-plural` - CLDR plural rules
- `math-interval-parser` - Interval notation parsing
- `mustache` - Template rendering

**Development:**
- `mocha` - Test framework
- `nyc` - Code coverage
- `eslint` - Linting
- `prettier` - Code formatting
- `express` - Example/test server
- `supertest` - HTTP testing

## Directory Structure

```
polyglot-i18n/
├── i18n.js                 # Main implementation (legacy JS)
├── index.js                # Module entry point
├── package.json            # Package configuration
├── README.adoc             # User documentation (AsciiDoc)
├── ROADMAP.adoc            # Development roadmap
├── SECURITY.md             # Security policy
├── CHANGELOG.md            # Version history
├── justfile                # Task runner (150+ recipes)
│
├── bindings/
│   └── rescript/           # ReScript bindings
│       ├── I18n.res        # Type definitions
│       └── I18n.resi       # Interface
│
├── deno/                   # Deno module
│   └── mod.ts              # Deno entry point
│
├── wasm/                   # WASM module (planned)
│   └── src/                # Rust source
│
├── config/
│   └── i18n.ncl            # Nickel type-safe configuration
│
├── container/
│   ├── Containerfile       # Multi-stage container build
│   ├── apko.yaml           # Declarative image build
│   └── nerdctl-compose.yaml # Container composition
│
├── guix.scm                # GNU Guix package definitions
├── channels.scm            # Guix channel configuration
├── manifest.scm            # Development environment manifest
├── flake.nix               # Nix flake (fallback)
│
├── locales/                # Default locale files
│   ├── en.json
│   ├── de.json
│   ├── fr.json
│   └── ...
│
├── examples/               # Usage examples
│   ├── express4-cookie/
│   ├── fastify/
│   ├── hono/
│   ├── koa/
│   ├── nestjs/
│   └── nextjs/
│
├── test/                   # Test suite
└── .github/                # CI/CD workflows
```

## Key Files

### i18n.js (Main Implementation)

**Location:** `/i18n.js`

**Purpose:** Core translation engine

**Key Functions:**
- `configure(options)` - Configure i18n settings
- `init(request, response, next)` - Middleware initialization
- `__(phrase, ...args)` - Basic translation
- `__n(singular, plural, count, ...args)` - Plural translations
- `__mf(phrase, ...args)` - MessageFormat translations
- `__l(phrase)` - List translations across locales
- `__h(phrase)` - Hash of translations
- `setLocale(locale)` - Set current locale
- `getLocale()` - Get current locale
- `getCatalog(locale)` - Get translation catalog

### Configuration Options

```javascript
{
  locales: ['en', 'de'],              // Supported locales
  fallbacks: { nl: 'de' },            // Fallback mappings
  defaultLocale: 'en',                // Default locale
  retryInDefaultLocale: false,        // Retry in default on missing
  cookie: 'locale',                   // Cookie name for locale
  header: 'accept-language',          // Header to check
  queryParameter: 'lang',             // Query param for locale
  directory: './locales',             // Translation files directory
  directoryPermissions: '755',        // Directory creation mode
  autoReload: false,                  // Watch files for changes
  updateFiles: true,                  // Write new keys to files
  syncFiles: false,                   // Sync across all locales
  indent: '\t',                       // JSON indentation
  extension: '.json',                 // File extension
  prefix: '',                         // Filename prefix
  objectNotation: false,              // Enable dot notation
  logDebugFn: function(msg) {},       // Debug logger
  logWarnFn: function(msg) {},        // Warn logger
  logErrorFn: function(msg) {},       // Error logger
  missingKeyFn: function(locale, key) {}, // Missing key handler
  register: global,                   // Object to bind API to
  api: { __: 't' },                   // API method aliases
  preserveLegacyCase: true,           // Preserve locale case
  staticCatalog: {},                  // Static translation object
  mustacheConfig: {},                 // Mustache configuration
  parser: JSON                        // Custom parser (YAML, etc)
}
```

## Development Workflow

### Using justfile (Recommended)

```bash
just                        # Show all available recipes
just dev                    # Start development environment
just test                   # Run all tests
just lint                   # Run linters
just build                  # Build all targets
```

### Using npm (Legacy)

```bash
npm install
npm test                    # Run test suite
npm run test-ci             # Run with coverage
npm run coverage            # Generate coverage report
```

### Using Guix (Preferred)

```bash
guix shell -m manifest.scm  # Enter development environment
just test                   # Run tests
```

### Using Nix (Fallback)

```bash
nix develop                 # Enter development environment
just test                   # Run tests
```

### Using Containers

```bash
just container-build        # Build container image
just container-run          # Run container
just container-dev          # Development container with shell
```

## Testing Strategy

**Test Framework:** Mocha + Should.js

**Test Location:** `/test/*.test.js`

**Coverage Tool:** NYC (Istanbul)

**Test Categories:**
- Unit tests for translation methods
- Integration tests with Express
- Locale file I/O tests
- Plural form tests
- MessageFormat tests
- Object notation tests
- Fallback mechanism tests

**Running Tests:**
```bash
just test                   # All tests
just test-unit              # Unit tests only
just test-integration       # Integration tests
just test-coverage          # With coverage report
```

## Important Patterns

### Instance vs Singleton

**Instance (Recommended):**
```javascript
const { I18n } = require('polyglot-i18n')
const i18n = new I18n({ locales: ['en', 'de'] })
```

**Singleton:**
```javascript
const i18n = require('polyglot-i18n')
i18n.configure({ locales: ['en', 'de'] })
```

### Middleware Integration

```javascript
const express = require('express')
const i18n = new I18n({ locales: ['en', 'de'], directory: './locales' })

app.use(i18n.init)  // Binds to req/res
```

### Translation Methods

**Basic:**
```javascript
__('Hello')                              // Simple
__('Hello %s', 'World')                  // sprintf
__('Hello {{name}}', { name: 'World' })  // mustache
```

**Plurals:**
```javascript
__n('%s cat', '%s cats', 1)              // Singular
__n('%s cat', '%s cats', 3)              // Plural
```

**MessageFormat:**
```javascript
__mf('{N, plural, one{# cat} other{# cats}}', { N: 1 })
```

### Object Notation

Enable with `objectNotation: true`

```javascript
// Translation file: { "greeting": { "formal": "Hello" } }
__('greeting.formal')                    // Access nested
__('greeting.formal:Hello')              // With default
```

## Configuration with Nickel

Type-safe configuration using Nickel:

```bash
# Generate JSON config from Nickel
just nickel-export development > config.json

# Validate configuration
just nickel-check

# Available presets
just nickel-export production
just nickel-export staging
just nickel-export test
```

## Container Workflows

```bash
# Build with apko (minimal, reproducible)
just container-apko-build

# Build with Containerfile
just container-build

# Run with nerdctl/podman/docker
just container-run

# Development environment
just container-dev

# Full stack with compose
just container-compose-up
```

## Guix/Nix Workflows

```bash
# Guix development shell
guix shell -m manifest.scm

# Build Guix package
guix build -f guix.scm

# Nix development shell
nix develop

# Build with Nix
nix build
```

## Code Style

- **Standard** + **Prettier** configuration
- ESLint with standard config
- Husky pre-commit hooks for linting
- Lint-staged for staged files only

## Important Conventions

1. **Locale Detection Order:** Query param → Cookie → Header → Default
2. **File Synchronization:** Only with `syncFiles: true`
3. **Auto-updating:** Only with `updateFiles: true`
4. **Scope Binding:** `req.locale`, `res.locale`, `res.locals.locale`
5. **Mustache Tags:** Configurable via `mustacheConfig.tags`

## Security Considerations

- See `SECURITY.md` for vulnerability reporting
- Avoid user input in translation keys (dynamic key creation)
- Sanitize variables in translations
- Use `{{var}}` (escaped) vs `{{{var}}}` (unescaped) carefully
- Container images use Chainguard Wolfi (minimal attack surface)

## Version Information

- **Current Version:** 0.16.0
- **Node Compatibility:** >= 18
- **Deno Compatibility:** >= 1.40
- **License:** MIT
- **Original:** Based on i18n-node by Marcus Spiegel

## Contributing

1. Fork repository
2. Create feature branch
3. Add tests for new features
4. Ensure tests pass: `just test`
5. Run linting: `just lint`
6. Submit pull request

## Related Resources

- **Repository:** https://github.com/hyperpolymath/polyglot-i18n
- **Original Project:** https://github.com/mashpie/i18n-node
- **MessageFormat Docs:** http://messageformat.github.io/
- **CLDR Plural Rules:** http://cldr.unicode.org/index/cldr-spec/plural-rules
- **ReScript Docs:** https://rescript-lang.org/docs/manual/latest/introduction
- **Nickel Docs:** https://nickel-lang.org/
- **GNU Guix:** https://guix.gnu.org/

## Tips for AI Development

1. **Read ROADMAP.adoc** for planned architecture changes
2. **Always read tests** in `/test` to understand expected behavior
3. **Check examples** in `/examples` for usage patterns
4. **Locale files** are in `/locales` - JSON format
5. **Main logic** is in `i18n.js` - being migrated to ReScript
6. **Breaking changes** require major version bump
7. **Performance** matters - translations are cached in memory
8. **Thread safety** - use instances for concurrent requests
9. **Use justfile** - `just` has 150+ recipes for common tasks
10. **Prefer Guix** - Guix-first, Nix fallback for reproducibility
11. **Container-first** - Chainguard Wolfi for production deployments

# CLAUDE.md - i18n-node-enhanced

## Project Overview

**i18n-node-enhanced** is a lightweight translation module with dynamic JSON storage for Node.js applications. It provides internationalization (i18n) and localization (l10n) support with a simple API.

**Key Features:**
- Dynamic JSON storage for translations
- Common `__('...')` syntax for translations
- Support for multiple locales
- Plural forms handling with CLDR rules
- MessageFormat support for advanced formatting
- Mustache template support
- sprintf-style formatting
- Object notation for hierarchical translations
- Auto-reload capability for development
- Compatible with Express, restify, and other frameworks

## Architecture

### Core Components

1. **i18n.js** (43KB) - Main module implementing the I18n class
   - Translation engine
   - Locale management
   - File I/O for JSON catalogs
   - API methods for translations

2. **index.js** - Entry point that exports the I18n class and singleton

3. **locales/** - Translation catalog storage (JSON files per locale)

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
i18n-node-enhanced/
├── i18n.js                 # Main implementation
├── index.js                # Module entry point
├── package.json            # Package configuration
├── README.md               # User documentation
├── SECURITY.md             # Security policy
├── CHANGELOG.md            # Version history
├── locales/                # Default locale files
│   ├── en.json
│   ├── de.json
│   ├── fr.json
│   └── ...
├── examples/               # Usage examples
│   ├── express4-cookie/
│   ├── express4-setLocale/
│   ├── node-http-autoreload/
│   └── singleton/
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

**Internal Methods:**
- `translate()` - Core translation logic
- `parseLoadLocales()` - Load locale files
- `writeFile()` - Persist translations to disk
- `guessLanguage()` - Detect locale from headers/cookies

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

### Setup

```bash
npm install
```

### Testing

```bash
npm test                    # Run test suite
npm run test-ci             # Run with coverage
npm run coverage            # Generate coverage report
```

### Linting

```bash
npx eslint i18n.js test/    # Check linting
npx eslint --fix i18n.js    # Auto-fix issues
```

### Examples

Each example in `/examples` is self-contained:

```bash
cd examples/express4-cookie
node index.js
npm test  # Run example tests
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

**Running Specific Tests:**
```bash
npx mocha test/i18n.configure.js
npx mocha test/i18n.translate.js
```

## Important Patterns

### Instance vs Singleton

**Instance (Recommended):**
```javascript
const { I18n } = require('i18n')
const i18n = new I18n({ locales: ['en', 'de'] })
```

**Singleton:**
```javascript
const i18n = require('i18n')
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

## Common Tasks

### Adding a New Locale

1. Add to configuration: `locales: ['en', 'de', 'fr']`
2. Create file: `locales/fr.json` with `{}`
3. Translations auto-populate on first use

### Customizing Plural Rules

Edit locale JSON file:
```json
{
  "%s dog": {
    "one": "one dog",
    "few": "a few dogs",
    "many": "many dogs",
    "other": "dogs"
  }
}
```

### Using Interval Notation

```json
{
  "dogs": {
    "other": "[0]no dog|[1]one dog|[2,5]some dogs|too many dogs"
  }
}
```

### Debug Logging

Enable with environment variable:
```bash
DEBUG=i18n:* node app.js        # All logs
DEBUG=i18n:warn,i18n:error node app.js  # Warnings/errors only
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

## Version Information

- **Current Version:** 0.15.3
- **Node Compatibility:** >= 10
- **License:** MIT

## Contributing

1. Fork repository
2. Create feature branch
3. Add tests for new features
4. Ensure tests pass: `npm test`
5. Run linting: `npx eslint --fix`
6. Submit pull request

## Related Resources

- **Main Repository:** https://github.com/mashpie/i18n-node
- **NPM Package:** https://www.npmjs.com/package/i18n
- **MessageFormat Docs:** http://messageformat.github.io/
- **CLDR Plural Rules:** http://cldr.unicode.org/index/cldr-spec/plural-rules

## Tips for AI Development

1. **Always read tests** in `/test` to understand expected behavior
2. **Check examples** in `/examples` for usage patterns
3. **Locale files** are in `/locales` - JSON format
4. **Main logic** is in `i18n.js` - ~1200 lines
5. **Breaking changes** require major version bump
6. **Backwards compatibility** is important for this library
7. **Performance** matters - translations are cached in memory
8. **Thread safety** - use instances for concurrent requests

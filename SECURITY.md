# Security Policy

## Supported Versions

We actively provide security updates for the following versions of polyglot-i18n:

| Version | Supported          | End of Life |
| ------- | ------------------ | ----------- |
| 0.16.x  | :white_check_mark: | -           |
| 0.15.x  | :white_check_mark: | TBD         |
| 0.14.x  | :warning: Critical fixes only | 2025-12-31 |
| < 0.14.0 | :x:               | EOL         |

## Reporting a Vulnerability

### How to Report

If you discover a security vulnerability in polyglot-i18n, please report it responsibly:

**Preferred Method:**
- GitHub Security Advisory: https://github.com/hyperpolymath/polyglot-i18n/security/advisories/new

**Alternative Method:**
- Open a private security advisory on GitHub

### What to Include

Please include the following information in your report:

1. **Description** of the vulnerability
2. **Steps to reproduce** the issue
3. **Potential impact** and attack scenarios
4. **Affected versions** (if known)
5. **Suggested fix** (if you have one)
6. **Your contact information** for follow-up

### Response Timeline

- **Acknowledgment**: Within 48 hours of report
- **Initial Assessment**: Within 7 days
- **Fix Timeline**:
  - Critical: 7-14 days
  - High: 14-30 days
  - Medium: 30-60 days
  - Low: Next planned release

### Disclosure Policy

We follow **coordinated disclosure**:

1. Vulnerability reported to maintainers
2. Fix developed and tested (private)
3. Security advisory prepared
4. Fix released with CVE (if applicable)
5. Public disclosure 7 days after fix release

---

## Security Best Practices

### 1. User Input in Translation Keys

**⚠️ CRITICAL: Never use unsanitized user input as translation keys**

```javascript
// ❌ DANGEROUS - Arbitrary file system access
const userInput = req.query.key; // Could be "../../../etc/passwd"
const translation = res.__(userInput);

// ✅ SAFE - Whitelist approach
const allowedKeys = ['welcome', 'goodbye', 'hello'];
const key = allowedKeys.includes(req.query.key) ? req.query.key : 'welcome';
const translation = res.__(key);
```

**Risk**: Directory traversal, arbitrary file reads/writes, catalog pollution

### 2. XSS Prevention in Translations

**⚠️ WARNING: Translations can contain HTML - sanitize user data in replacements**

```javascript
// ❌ DANGEROUS - XSS vulnerability
const username = req.query.name; // Could be "<script>alert('XSS')</script>"
const message = res.__('Welcome {{name}}', { name: username });
// Output: Welcome <script>alert('XSS')</script>

// ✅ SAFE - Escape HTML
const escapeHtml = (str) => str.replace(/[&<>"']/g, (m) => ({
  '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;'
})[m]);

const username = escapeHtml(req.query.name);
const message = res.__('Welcome {{name}}', { name: username });

// ✅ BETTER - Use templating engine with auto-escaping
// In EJS: <%= __('Welcome {{name}}', { name: username }) %>
// In Handlebars: {{__ 'Welcome {{name}}' name=username}}
```

### 3. Mustache Unescaped Variables

**⚠️ WARNING: Triple-mustache syntax bypasses HTML escaping**

```javascript
// ❌ DANGEROUS - Allows raw HTML injection
const userBio = req.body.bio; // Could contain malicious HTML
const message = res.__('Bio: {{{bio}}}', { bio: userBio });

// ✅ SAFE - Use double mustache (escaped)
const message = res.__('Bio: {{bio}}', { bio: userBio });

// ✅ ACCEPTABLE - Only if you control the content
const trustedHTML = '<strong>Admin</strong>';
const message = res.__('User: {{{role}}}', { role: trustedHTML });
```

### 4. File System Security

**⚠️ WARNING: File write operations can be exploited**

```javascript
// ✅ SAFE Configuration
i18n.configure({
  directory: path.resolve(__dirname, 'locales'), // Use absolute paths
  updateFiles: false, // Disable in production
  autoReload: false,  // Disable in production
  directoryPermissions: '755', // Restrictive permissions

  // Validate locale names to prevent traversal
  locales: ['en', 'de', 'fr'] // Explicit whitelist
});

// ❌ DANGEROUS - Production settings
i18n.configure({
  updateFiles: true,  // Allows arbitrary file writes
  autoReload: true,   // Performance + security risk
  locales: undefined  // Auto-detects from directory (risky)
});
```

### 5. Locale Injection Prevention

**⚠️ WARNING: Validate locale values from user input**

```javascript
// ❌ DANGEROUS - Arbitrary locale setting
app.get('/set-locale/:locale', (req, res) => {
  req.setLocale(req.params.locale); // Could be "../../../etc/passwd"
  res.redirect('/');
});

// ✅ SAFE - Validate against allowed locales
app.get('/set-locale/:locale', (req, res) => {
  const locale = req.params.locale;
  const allowedLocales = i18n.getLocales();

  if (allowedLocales.includes(locale)) {
    req.setLocale(locale);
  } else {
    // Log potential attack
    console.warn(`Invalid locale attempted: ${locale}`);
    req.setLocale(i18n.getLocale()); // Keep current locale
  }

  res.redirect('/');
});
```

### 6. MessageFormat Injection

**⚠️ WARNING: User-controlled MessageFormat strings can cause DoS or crashes**

```javascript
// ❌ DANGEROUS - User controls MessageFormat syntax
const userFormat = req.body.format; // "{x, select, , {,}} ..." - malformed
const message = res.__mf(userFormat, { x: 'test' }); // May crash

// ✅ SAFE - Only use predefined MessageFormat strings
const formats = {
  items: '{N, plural, one{# item} other{# items}}',
  users: '{N, plural, one{# user} other{# users}}'
};

const formatKey = req.query.type;
const format = formats[formatKey] || formats.items;
const message = res.__mf(format, { N: count });
```

### 7. Cookie Security

**✅ Recommended cookie configuration:**

```javascript
// Set locale cookie securely
res.cookie('locale', locale, {
  httpOnly: true,      // Prevents XSS access
  secure: true,        // HTTPS only (production)
  sameSite: 'strict',  // CSRF protection
  maxAge: 31536000000, // 1 year
  signed: true         // Requires cookie-parser secret
});

// Express middleware
app.use(cookieParser('your-secret-key-here')); // Use strong secret
i18n.configure({
  cookie: 'locale'
});
```

### 8. Prototype Pollution Prevention

**⚠️ WARNING: Object notation can be vulnerable to prototype pollution**

```javascript
// ❌ POTENTIALLY DANGEROUS with objectNotation: true
const key = req.query.key; // Could be "__proto__.polluted"
const value = res.__(key);

// ✅ SAFE - Disable object notation if using user input
i18n.configure({
  objectNotation: false // Disable if keys come from user input
});

// ✅ SAFE - Validate keys
const allowedPattern = /^[a-z0-9._-]+$/i;
if (!allowedPattern.test(key)) {
  throw new Error('Invalid translation key');
}
```

### 9. Denial of Service (DoS) Prevention

**⚠️ WARNING: Large translation files and excessive operations**

```javascript
// ✅ Recommendations:
// 1. Limit locale file sizes (< 1MB recommended)
// 2. Use staticCatalog in production (faster, safer)
// 3. Rate-limit locale switching endpoints
// 4. Cache translations in memory (default behavior)

// Production configuration
i18n.configure({
  staticCatalog: {
    en: require('./locales/en.json'),
    de: require('./locales/de.json')
  },
  updateFiles: false,
  autoReload: false
});

// Rate limiting for locale switching
const rateLimit = require('express-rate-limit');
app.use('/set-locale', rateLimit({
  windowMs: 15 * 60 * 1000, // 15 minutes
  max: 10 // 10 requests per window
}));
```

### 10. Dependency Security

**Keep dependencies updated:**

```bash
# Check for vulnerabilities
npm audit

# Fix automatically
npm audit fix

# Update dependencies
npm update

# Check outdated packages
npm outdated
```

---

## Security Checklist for Production

- [ ] Set `updateFiles: false` in production
- [ ] Set `autoReload: false` in production
- [ ] Use `staticCatalog` for better performance and security
- [ ] Validate all user input before using in translations
- [ ] Escape HTML in translation replacements
- [ ] Use absolute paths for `directory` configuration
- [ ] Whitelist allowed locales, validate locale switching
- [ ] Use secure cookie settings (httpOnly, secure, sameSite)
- [ ] Never use user input as translation keys
- [ ] Keep dependencies updated (`npm audit`)
- [ ] Disable object notation if keys come from user input
- [ ] Rate-limit locale switching endpoints
- [ ] Use Content Security Policy (CSP) headers
- [ ] Regularly review and sanitize translation files
- [ ] Monitor for suspicious locale file changes

---

## Known Security Issues

### Resolved

- **CVE-2025-57353** - @messageformat/runtime Prototype Pollution
  - **Status**: Fixed in v0.15.3
  - **Action**: Updated @messageformat/runtime to 3.0.2
  - **Severity**: Medium

### Active Monitoring

We continuously monitor:
- Dependency vulnerabilities via npm audit
- Security advisories for MessageFormat and Mustache
- Community-reported issues

---

## Security Headers Recommendation

When using i18n in web applications, implement these security headers:

```javascript
// Express helmet middleware
const helmet = require('helmet');

app.use(helmet({
  contentSecurityPolicy: {
    directives: {
      defaultSrc: ["'self'"],
      scriptSrc: ["'self'"], // No inline scripts
      styleSrc: ["'self'", "'unsafe-inline'"] // For dynamic styles only
    }
  },
  hsts: {
    maxAge: 31536000,
    includeSubDomains: true,
    preload: true
  }
}));
```

---

## Additional Resources

- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [OWASP i18n Security Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Internationalization_Security_Cheat_Sheet.html)
- [Node.js Security Best Practices](https://nodejs.org/en/docs/guides/security/)
- [Express Security Best Practices](https://expressjs.com/en/advanced/best-practice-security.html)

---

## Contact

For security-related questions or concerns:
- **Security Issues**: Use GitHub Security Advisory or email security contact
- **General Questions**: Open a GitHub issue
- **Commercial Support**: Contact through Tidelift

Last Updated: 2025-12-07

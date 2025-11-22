/**
 * Oracle ERP Cloud Integration Adapter
 * Supports: Oracle Fusion Applications, OIC, OTBI, FBDI
 */

const { I18n } = require('../../index');
const { I18nAuditSystem } = require('../../audit/forensics');

class OracleERPI18nAdapter {
  constructor(config = {}) {
    this.config = {
      host: config.host || process.env.ORACLE_HOST,
      username: config.username || process.env.ORACLE_USERNAME,
      password: config.password || process.env.ORACLE_PASSWORD,
      tenant: config.tenant || process.env.ORACLE_TENANT,
      locales: config.locales || ['en_US', 'de_DE', 'fr_FR', 'es_ES'],
      batchSize: config.batchSize || 1000,
      auditEnabled: config.auditEnabled !== false,
      ...config
    };

    this.i18n = new I18n(config.i18n || {});

    if (this.config.auditEnabled) {
      this.audit = new I18nAuditSystem({
        enabled: true,
        logDir: config.auditLogDir || './audit-logs/oracle'
      });
    }

    // Oracle language code mappings
    this.languageMapping = {
      'en_US': 'US', 'en_GB': 'GB', 'de_DE': 'D', 'fr_FR': 'F',
      'es_ES': 'E', 'es_MX': 'ESA', 'pt_BR': 'PTB', 'it_IT': 'I',
      'ja_JP': 'JA', 'zh_CN': 'ZHS', 'zh_TW': 'ZHT', 'ko_KR': 'KO',
      'nl_NL': 'NL', 'pl_PL': 'PL', 'ru_RU': 'RU', 'ar_SA': 'AR'
    };
  }

  /**
   * Map Oracle language code to i18n locale
   */
  mapOracleLanguageToLocale(oracleCode) {
    const reverseMapping = Object.entries(this.languageMapping)
      .reduce((acc, [locale, code]) => {
        acc[code] = locale;
        return acc;
      }, {});

    return reverseMapping[oracleCode] || 'en_US';
  }

  /**
   * Map i18n locale to Oracle language code
   */
  mapLocaleToOracleLanguage(locale) {
    return this.languageMapping[locale] || 'US';
  }

  /**
   * Translate text for Oracle Fusion Applications
   */
  async translateFusionText(messageCode, locale, params = {}) {
    const targetLocale = this.mapOracleLanguageToLocale(locale);
    this.i18n.setLocale(targetLocale);

    const translated = this.i18n.__(messageCode, params);

    if (this.audit) {
      this.audit.logTranslation({
        system: 'Oracle Fusion',
        operation: 'translateFusionText',
        messageCode,
        locale: targetLocale,
        oracleLanguage: locale,
        result: translated
      });
    }

    return translated;
  }

  /**
   * Generate FBDI (File-Based Data Import) format for bulk translation upload
   */
  generateFBDIFile(locale, options = {}) {
    const catalog = this.i18n.getCatalog(locale);
    const oracleCode = this.mapLocaleToOracleLanguage(locale);

    const fbdiRows = [];

    // FBDI header
    fbdiRows.push([
      'MESSAGE_CODE',
      'LANGUAGE_CODE',
      'MESSAGE_TEXT',
      'DESCRIPTION',
      'APPLICATION_NAME',
      'MODULE',
      'SOURCE_LANG'
    ].join(','));

    // Flatten catalog and generate FBDI rows
    const flatten = (obj, prefix = '') => {
      Object.entries(obj).forEach(([key, value]) => {
        const fullKey = prefix ? `${prefix}.${key}` : key;

        if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
          flatten(value, fullKey);
        } else {
          const row = [
            this.escapeFBDIValue(fullKey),
            oracleCode,
            this.escapeFBDIValue(String(value)),
            this.escapeFBDIValue(options.description || ''),
            this.escapeFBDIValue(options.applicationName || 'CUSTOM'),
            this.escapeFBDIValue(options.module || 'I18N'),
            'US' // Source language
          ];
          fbdiRows.push(row.join(','));
        }
      });
    };

    flatten(catalog);

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'export',
        system: 'Oracle FBDI',
        locale,
        format: 'FBDI',
        rowCount: fbdiRows.length - 1
      });
    }

    return fbdiRows.join('\n');
  }

  /**
   * Generate OIC (Oracle Integration Cloud) payload
   */
  generateOICPayload(locale, messageKeys = []) {
    const catalog = this.i18n.getCatalog(locale);
    const oracleCode = this.mapLocaleToOracleLanguage(locale);

    const messages = messageKeys.length > 0
      ? messageKeys.map(key => ({ key, value: catalog[key] || key }))
      : Object.entries(catalog).map(([key, value]) => ({ key, value }));

    const payload = {
      Header: {
        TenantId: this.config.tenant,
        Language: oracleCode,
        Timestamp: new Date().toISOString(),
        RequestId: require('crypto').randomBytes(16).toString('hex')
      },
      Messages: messages.map(msg => ({
        MessageCode: msg.key,
        MessageText: msg.value,
        Language: oracleCode,
        EffectiveDate: new Date().toISOString(),
        Status: 'ACTIVE'
      }))
    };

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'export',
        system: 'Oracle OIC',
        locale,
        format: 'OIC_JSON',
        messageCount: messages.length,
        requestId: payload.Header.RequestId
      });
    }

    return payload;
  }

  /**
   * Export to OTBI (Oracle Transactional Business Intelligence) format
   */
  exportToOTBI(locale, options = {}) {
    const catalog = this.i18n.getCatalog(locale);
    const oracleCode = this.mapLocaleToOracleLanguage(locale);

    const otbiData = {
      SubjectArea: options.subjectArea || 'Common - Translation',
      ReportName: options.reportName || `Translations_${locale}`,
      Language: oracleCode,
      Data: []
    };

    const flatten = (obj, prefix = '') => {
      Object.entries(obj).forEach(([key, value]) => {
        const fullKey = prefix ? `${prefix}.${key}` : key;

        if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
          flatten(value, fullKey);
        } else {
          otbiData.Data.push({
            MessageCode: fullKey,
            MessageText: String(value),
            Language: oracleCode,
            Module: options.module || 'CUSTOM',
            LastUpdateDate: new Date().toISOString()
          });
        }
      });
    };

    flatten(catalog);

    return otbiData;
  }

  /**
   * Batch translation for Oracle REST API
   */
  async batchTranslate(requests, locale) {
    const results = [];
    const targetLocale = this.mapOracleLanguageToLocale(locale);
    this.i18n.setLocale(targetLocale);

    // Process in batches
    for (let i = 0; i < requests.length; i += this.config.batchSize) {
      const batch = requests.slice(i, i + this.config.batchSize);

      const batchResults = batch.map(req => ({
        MessageCode: req.messageCode,
        Language: locale,
        TranslatedText: this.i18n.__(req.messageCode, req.params || {}),
        Status: 'SUCCESS',
        Timestamp: new Date().toISOString()
      }));

      results.push(...batchResults);
    }

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'batch_translation',
        system: 'Oracle ERP',
        locale: targetLocale,
        totalRequests: requests.length,
        batchCount: Math.ceil(requests.length / this.config.batchSize)
      });
    }

    return {
      TotalRequests: requests.length,
      SuccessCount: results.length,
      Results: results
    };
  }

  /**
   * Import translations from Oracle Fusion export
   */
  async importFromFusion(fusionData, locale) {
    const updates = {};

    fusionData.Messages.forEach(msg => {
      updates[msg.MessageCode] = msg.MessageText;
    });

    if (this.audit) {
      this.audit.logCatalogModification({
        locale,
        operation: 'bulk_import',
        source: 'Oracle Fusion',
        keysUpdated: Object.keys(updates).length,
        automated: true
      });
    }

    return {
      locale,
      imported: Object.keys(updates).length,
      updates
    };
  }

  /**
   * Express middleware for Oracle Fusion Applications
   */
  fusionMiddleware() {
    return (req, res, next) => {
      const oracleLanguage = req.headers['oracle-language'] ||
                           req.query.oracleLanguage ||
                           'US';

      const locale = this.mapOracleLanguageToLocale(oracleLanguage);
      this.i18n.setLocale(req, locale);

      // Add Oracle-specific translation helpers
      req.translateFusion = (messageCode, params) => {
        return this.translateFusionText(messageCode, oracleLanguage, params);
      };

      res.locals.oracleLanguage = oracleLanguage;
      res.locals.fusionTranslate = req.translateFusion;

      next();
    };
  }

  /**
   * Escape CSV values for FBDI format
   */
  escapeFBDIValue(value) {
    if (value === null || value === undefined) return '';
    const str = String(value);
    if (str.includes(',') || str.includes('"') || str.includes('\n')) {
      return `"${str.replace(/"/g, '""')}"`;
    }
    return str;
  }

  /**
   * Validate Oracle translation payload
   */
  validateOraclePayload(payload) {
    const errors = [];

    if (!payload.Header) {
      errors.push('Missing Header object');
    } else {
      if (!payload.Header.Language) errors.push('Missing Header.Language');
      if (!payload.Header.TenantId) errors.push('Missing Header.TenantId');
    }

    if (!payload.Messages || !Array.isArray(payload.Messages)) {
      errors.push('Missing or invalid Messages array');
    } else {
      payload.Messages.forEach((msg, idx) => {
        if (!msg.MessageCode) errors.push(`Message[${idx}]: Missing MessageCode`);
        if (!msg.MessageText) errors.push(`Message[${idx}]: Missing MessageText`);
      });
    }

    return {
      valid: errors.length === 0,
      errors
    };
  }
}

module.exports = { OracleERPI18nAdapter };

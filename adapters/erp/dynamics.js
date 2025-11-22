/**
 * Microsoft Dynamics 365 Integration Adapter
 * Supports: Finance & Operations, Business Central, Power Platform, Dataverse
 */

const { I18n } = require('../../index');
const { I18nAuditSystem } = require('../../audit/forensics');

class DynamicsI18nAdapter {
  constructor(config = {}) {
    this.config = {
      tenantId: config.tenantId || process.env.DYNAMICS_TENANT_ID,
      clientId: config.clientId || process.env.DYNAMICS_CLIENT_ID,
      clientSecret: config.clientSecret || process.env.DYNAMICS_CLIENT_SECRET,
      environment: config.environment || process.env.DYNAMICS_ENVIRONMENT,
      apiVersion: config.apiVersion || 'v9.2',
      locales: config.locales || ['en-US', 'de-DE', 'fr-FR', 'es-ES'],
      auditEnabled: config.auditEnabled !== false,
      ...config
    };

    this.i18n = new I18n(config.i18n || {});

    if (this.config.auditEnabled) {
      this.audit = new I18nAuditSystem({
        enabled: true,
        logDir: config.auditLogDir || './audit-logs/dynamics'
      });
    }

    // Dynamics 365 LCID (Locale ID) mappings
    this.lcidMapping = {
      'en-US': 1033, 'en-GB': 2057, 'de-DE': 1031, 'fr-FR': 1036,
      'es-ES': 3082, 'es-MX': 2058, 'pt-BR': 1046, 'it-IT': 1040,
      'ja-JP': 1041, 'zh-CN': 2052, 'zh-TW': 1028, 'ko-KR': 1042,
      'nl-NL': 1043, 'pl-PL': 1045, 'ru-RU': 1049, 'ar-SA': 1025,
      'da-DK': 1030, 'fi-FI': 1035, 'sv-SE': 1053, 'nb-NO': 1044,
      'tr-TR': 1055, 'cs-CZ': 1029, 'hu-HU': 1038, 'ro-RO': 1048
    };
  }

  /**
   * Map Dynamics LCID to i18n locale
   */
  mapLCIDToLocale(lcid) {
    const reverseMapping = Object.entries(this.lcidMapping)
      .reduce((acc, [locale, id]) => {
        acc[id] = locale;
        return acc;
      }, {});

    return reverseMapping[lcid] || 'en-US';
  }

  /**
   * Map i18n locale to Dynamics LCID
   */
  mapLocaleToLCID(locale) {
    return this.lcidMapping[locale] || 1033;
  }

  /**
   * Generate Dynamics 365 label file (XML) for Finance & Operations
   */
  generateLabelFile(locale, labelFileId, options = {}) {
    const catalog = this.i18n.getCatalog(locale);
    const lcid = this.mapLocaleToLCID(locale);

    let xml = `<?xml version="1.0" encoding="utf-8"?>\n`;
    xml += `<AxLabelFile xmlns:i="http://www.w3.org/2001/XMLSchema-instance">\n`;
    xml += `  <LabelFileId>${this.escapeXML(labelFileId)}</LabelFileId>\n`;
    xml += `  <LabelContentVersion>1.0.0</LabelContentVersion>\n`;
    xml += `  <LanguageId>${lcid}</LanguageId>\n`;
    xml += `  <Labels>\n`;

    const flatten = (obj, prefix = '') => {
      Object.entries(obj).forEach(([key, value]) => {
        const fullKey = prefix ? `${prefix}_${key}` : key;

        if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
          flatten(value, fullKey);
        } else {
          xml += `    <Label>\n`;
          xml += `      <LabelId>${this.escapeXML(fullKey)}</LabelId>\n`;
          xml += `      <LabelText>${this.escapeXML(String(value))}</LabelText>\n`;
          xml += `      <Comment>${this.escapeXML(options.comment || '')}</Comment>\n`;
          xml += `    </Label>\n`;
        }
      });
    };

    flatten(catalog);

    xml += `  </Labels>\n`;
    xml += `</AxLabelFile>`;

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'export',
        system: 'Dynamics 365 F&O',
        locale,
        format: 'LabelFile_XML',
        labelFileId
      });
    }

    return xml;
  }

  /**
   * Generate Dataverse localized string for custom entities
   */
  generateDataverseLocalizedString(entityName, attributeName, locale, value) {
    const lcid = this.mapLocaleToLCID(locale);

    return {
      '@odata.type': 'Microsoft.Dynamics.CRM.LocalizedLabel',
      Label: value,
      LanguageCode: lcid,
      EntityLogicalName: entityName,
      AttributeLogicalName: attributeName,
      IsManaged: false,
      MetadataId: null
    };
  }

  /**
   * Generate Power Platform resource file (RESX)
   */
  generatePowerPlatformResx(locale, options = {}) {
    const catalog = this.i18n.getCatalog(locale);

    let resx = `<?xml version="1.0" encoding="utf-8"?>\n`;
    resx += `<root>\n`;
    resx += `  <resheader name="resmimetype">\n`;
    resx += `    <value>text/microsoft-resx</value>\n`;
    resx += `  </resheader>\n`;
    resx += `  <resheader name="version">\n`;
    resx += `    <value>2.0</value>\n`;
    resx += `  </resheader>\n`;
    resx += `  <resheader name="reader">\n`;
    resx += `    <value>System.Resources.ResXResourceReader, System.Windows.Forms</value>\n`;
    resx += `  </resheader>\n`;
    resx += `  <resheader name="writer">\n`;
    resx += `    <value>System.Resources.ResXResourceWriter, System.Windows.Forms</value>\n`;
    resx += `  </resheader>\n`;

    const flatten = (obj, prefix = '') => {
      Object.entries(obj).forEach(([key, value]) => {
        const fullKey = prefix ? `${prefix}.${key}` : key;

        if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
          flatten(value, fullKey);
        } else {
          resx += `  <data name="${this.escapeXML(fullKey)}" xml:space="preserve">\n`;
          resx += `    <value>${this.escapeXML(String(value))}</value>\n`;
          if (options.comments && options.comments[fullKey]) {
            resx += `    <comment>${this.escapeXML(options.comments[fullKey])}</comment>\n`;
          }
          resx += `  </data>\n`;
        }
      });
    };

    flatten(catalog);

    resx += `</root>`;

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'export',
        system: 'Power Platform',
        locale,
        format: 'RESX'
      });
    }

    return resx;
  }

  /**
   * Generate Business Central translation file
   */
  generateBusinessCentralTranslation(locale, options = {}) {
    const catalog = this.i18n.getCatalog(locale);
    const lcid = this.mapLocaleToLCID(locale);

    const translation = {
      Culture: locale,
      LCID: lcid,
      Translations: []
    };

    const flatten = (obj, prefix = '') => {
      Object.entries(obj).forEach(([key, value]) => {
        const fullKey = prefix ? `${prefix}.${key}` : key;

        if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
          flatten(value, fullKey);
        } else {
          translation.Translations.push({
            Key: fullKey,
            Value: String(value),
            ObjectType: options.objectType || 'Page',
            ObjectId: options.objectId || 0,
            ControlName: fullKey
          });
        }
      });
    };

    flatten(catalog);

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'export',
        system: 'Dynamics 365 Business Central',
        locale,
        format: 'JSON',
        translationCount: translation.Translations.length
      });
    }

    return translation;
  }

  /**
   * Translate text for Dynamics 365 applications
   */
  async translateDynamicsText(key, locale, params = {}) {
    const targetLocale = this.mapLCIDToLocale(locale);
    this.i18n.setLocale(targetLocale);

    const translated = this.i18n.__(key, params);

    if (this.audit) {
      this.audit.logTranslation({
        system: 'Dynamics 365',
        operation: 'translateDynamicsText',
        key,
        locale: targetLocale,
        lcid: locale,
        result: translated
      });
    }

    return translated;
  }

  /**
   * Batch translation for Dynamics Web API
   */
  async batchTranslate(requests, locale) {
    const results = [];
    const targetLocale = this.mapLCIDToLocale(locale);
    this.i18n.setLocale(targetLocale);

    const batchResponse = {
      Responses: requests.map(req => ({
        RequestId: req.requestId || require('crypto').randomBytes(8).toString('hex'),
        Key: req.key,
        LCID: locale,
        TranslatedText: this.i18n.__(req.key, req.parameters || {}),
        Status: 'Success',
        Timestamp: new Date().toISOString()
      }))
    };

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'batch_translation',
        system: 'Dynamics 365',
        locale: targetLocale,
        lcid: locale,
        requestCount: requests.length
      });
    }

    return batchResponse;
  }

  /**
   * Export to Dynamics 365 Web API format
   */
  exportToWebAPI(locale, entityName) {
    const catalog = this.i18n.getCatalog(locale);
    const lcid = this.mapLocaleToLCID(locale);

    const webApiPayload = {
      '@odata.context': `${this.config.environment}/api/data/${this.config.apiVersion}/$metadata#${entityName}`,
      value: []
    };

    const flatten = (obj, prefix = '') => {
      Object.entries(obj).forEach(([key, value]) => {
        const fullKey = prefix ? `${prefix}.${key}` : key;

        if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
          flatten(value, fullKey);
        } else {
          webApiPayload.value.push({
            key: fullKey,
            value: String(value),
            lcid: lcid,
            locale: locale,
            entityname: entityName,
            createdon: new Date().toISOString()
          });
        }
      });
    };

    flatten(catalog);

    return webApiPayload;
  }

  /**
   * Import translations from Dynamics 365 export
   */
  async importFromDynamics(dynamicsData, locale) {
    const updates = {};

    if (Array.isArray(dynamicsData.Translations)) {
      // Business Central format
      dynamicsData.Translations.forEach(trans => {
        updates[trans.Key] = trans.Value;
      });
    } else if (Array.isArray(dynamicsData.value)) {
      // Web API format
      dynamicsData.value.forEach(item => {
        updates[item.key] = item.value;
      });
    }

    if (this.audit) {
      this.audit.logCatalogModification({
        locale,
        operation: 'bulk_import',
        source: 'Dynamics 365',
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
   * Express middleware for Dynamics 365 applications
   */
  dynamicsMiddleware() {
    return (req, res, next) => {
      const lcid = parseInt(req.headers['dynamics-lcid'] ||
                           req.query.lcid ||
                           '1033');

      const locale = this.mapLCIDToLocale(lcid);
      this.i18n.setLocale(req, locale);

      // Add Dynamics-specific translation helpers
      req.translateDynamics = (key, params) => {
        return this.translateDynamicsText(key, lcid, params);
      };

      req.getLocalizedString = (entityName, attributeName, value) => {
        return this.generateDataverseLocalizedString(entityName, attributeName, locale, value);
      };

      res.locals.dynamicsLCID = lcid;
      res.locals.dynamicsLocale = locale;
      res.locals.dynamicsTranslate = req.translateDynamics;

      next();
    };
  }

  /**
   * Generate Power Apps control property localization
   */
  generatePowerAppsControlProperties(controlName, locale, properties) {
    const lcid = this.mapLocaleToLCID(locale);

    return {
      ControlName: controlName,
      LCID: lcid,
      Locale: locale,
      Properties: Object.entries(properties).map(([propName, key]) => ({
        PropertyName: propName,
        LocalizedValue: this.i18n.__(key),
        TranslationKey: key
      }))
    };
  }

  /**
   * Validate Dynamics 365 payload
   */
  validateDynamicsPayload(payload) {
    const errors = [];

    if (!payload.LCID && !payload.lcid) {
      errors.push('Missing LCID (Language Code ID)');
    }

    if (!payload.Translations && !payload.value) {
      errors.push('Missing Translations or value array');
    }

    return {
      valid: errors.length === 0,
      errors
    };
  }

  /**
   * Escape XML special characters
   */
  escapeXML(str) {
    if (!str) return '';
    return String(str)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;')
      .replace(/'/g, '&apos;');
  }
}

module.exports = { DynamicsI18nAdapter };

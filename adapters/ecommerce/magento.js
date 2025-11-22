/**
 * Magento Integration Adapter
 * Supports: Store view translations, Product attributes, CMS block localization
 */

const { I18n } = require('../../index');
const { I18nAuditSystem } = require('../../audit/forensics');

class MagentoI18nAdapter {
  constructor(config = {}) {
    this.config = {
      baseUrl: config.baseUrl || process.env.MAGENTO_BASE_URL,
      accessToken: config.accessToken || process.env.MAGENTO_ACCESS_TOKEN,
      adminPath: config.adminPath || 'admin',
      locales: config.locales || ['en_US', 'de_DE', 'fr_FR', 'es_ES'],
      defaultLocale: config.defaultLocale || 'en_US',
      auditEnabled: config.auditEnabled !== false,
      ...config
    };

    this.i18n = new I18n(config.i18n || {});

    if (this.config.auditEnabled) {
      this.audit = new I18nAuditSystem({
        enabled: true,
        logDir: config.auditLogDir || './audit-logs/magento'
      });
    }

    // Magento locale format mappings
    this.localeMapping = {
      'en-US': 'en_US', 'en-GB': 'en_GB', 'de-DE': 'de_DE', 'fr-FR': 'fr_FR',
      'es-ES': 'es_ES', 'es-MX': 'es_MX', 'pt-BR': 'pt_BR', 'it-IT': 'it_IT',
      'ja-JP': 'ja_JP', 'zh-CN': 'zh_Hans_CN', 'zh-TW': 'zh_Hant_TW',
      'ko-KR': 'ko_KR', 'nl-NL': 'nl_NL', 'pl-PL': 'pl_PL', 'ru-RU': 'ru_RU',
      'sv-SE': 'sv_SE', 'da-DK': 'da_DK', 'fi-FI': 'fi_FI', 'nb-NO': 'nb_NO'
    };
  }

  /**
   * Map Magento locale to i18n locale
   */
  mapMagentoLocaleToI18n(magentoLocale) {
    const reverseMapping = Object.entries(this.localeMapping)
      .reduce((acc, [i18nLocale, magentoFormat]) => {
        acc[magentoFormat] = i18nLocale;
        return acc;
      }, {});

    return reverseMapping[magentoLocale] || this.config.defaultLocale;
  }

  /**
   * Map i18n locale to Magento format
   */
  mapI18nLocaleToMagento(locale) {
    return this.localeMapping[locale] || 'en_US';
  }

  /**
   * Generate Magento CSV translation file
   */
  generateMagentoCSV(locale, moduleName = 'Custom') {
    const catalog = this.i18n.getCatalog(locale);
    const magentoLocale = this.mapI18nLocaleToMagento(locale);

    let csv = '';

    const flatten = (obj, prefix = '') => {
      Object.entries(obj).forEach(([key, value]) => {
        const fullKey = prefix ? `${prefix}.${key}` : key;

        if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
          flatten(value, fullKey);
        } else {
          // Magento CSV format: "Original string","Translated string","module"
          const original = this.escapeCSV(fullKey);
          const translation = this.escapeCSV(String(value));
          csv += `"${original}","${translation}","${moduleName}"\n`;
        }
      });
    };

    flatten(catalog);

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'export',
        system: 'Magento',
        locale,
        magentoLocale,
        format: 'CSV',
        module: moduleName
      });
    }

    return csv;
  }

  /**
   * Generate Magento i18n JSON dictionary
   */
  generateMagentoI18nJSON(locale) {
    const catalog = this.i18n.getCatalog(locale);
    const magentoLocale = this.mapI18nLocaleToMagento(locale);

    const i18nDict = {};

    const flatten = (obj, prefix = '') => {
      Object.entries(obj).forEach(([key, value]) => {
        const fullKey = prefix ? `${prefix}.${key}` : key;

        if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
          flatten(value, fullKey);
        } else {
          i18nDict[fullKey] = String(value);
        }
      });
    };

    flatten(catalog);

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'export',
        system: 'Magento i18n',
        locale,
        magentoLocale,
        format: 'JSON'
      });
    }

    return i18nDict;
  }

  /**
   * Translate Magento product attribute
   */
  async translateProductAttribute(attribute, locale, storeId) {
    this.i18n.setLocale(locale);
    const magentoLocale = this.mapI18nLocaleToMagento(locale);

    const translatedAttribute = {
      attribute_code: attribute.attribute_code,
      store_id: storeId,
      locale: magentoLocale,
      frontend_label: this.i18n.__(`attributes.${attribute.attribute_code}.label`, {
        fallback: attribute.frontend_label
      }),
      options: attribute.options ? attribute.options.map(option => ({
        value: option.value,
        label: this.i18n.__(`attributes.${attribute.attribute_code}.options.${option.value}`, {
          fallback: option.label
        })
      })) : []
    };

    if (this.audit) {
      this.audit.logTranslation({
        system: 'Magento Product Attribute',
        operation: 'translateProductAttribute',
        attributeCode: attribute.attribute_code,
        locale,
        storeId
      });
    }

    return translatedAttribute;
  }

  /**
   * Generate Magento CMS block translation
   */
  async translateCMSBlock(block, locale, storeId) {
    this.i18n.setLocale(locale);
    const magentoLocale = this.mapI18nLocaleToMagento(locale);

    return {
      identifier: block.identifier,
      store_id: storeId,
      locale: magentoLocale,
      title: this.i18n.__(`cms.blocks.${block.identifier}.title`, {
        fallback: block.title
      }),
      content: this.i18n.__(`cms.blocks.${block.identifier}.content`, {
        fallback: block.content
      }),
      is_active: block.is_active
    };
  }

  /**
   * Generate Magento CMS page translation
   */
  async translateCMSPage(page, locale, storeId) {
    this.i18n.setLocale(locale);
    const magentoLocale = this.mapI18nLocaleToMagento(locale);

    return {
      identifier: page.identifier,
      store_id: storeId,
      locale: magentoLocale,
      title: this.i18n.__(`cms.pages.${page.identifier}.title`, {
        fallback: page.title
      }),
      content_heading: this.i18n.__(`cms.pages.${page.identifier}.heading`, {
        fallback: page.content_heading
      }),
      content: this.i18n.__(`cms.pages.${page.identifier}.content`, {
        fallback: page.content
      }),
      meta_title: this.i18n.__(`cms.pages.${page.identifier}.meta.title`, {
        fallback: page.meta_title
      }),
      meta_description: this.i18n__(`cms.pages.${page.identifier}.meta.description`, {
        fallback: page.meta_description
      }),
      meta_keywords: this.i18n__(`cms.pages.${page.identifier}.meta.keywords`, {
        fallback: page.meta_keywords
      })
    };
  }

  /**
   * Generate Magento email template translation
   */
  translateEmailTemplate(templateCode, locale, variables = {}) {
    this.i18n.setLocale(locale);
    const magentoLocale = this.mapI18nLocaleToMagento(locale);

    return {
      template_code: templateCode,
      locale: magentoLocale,
      template_subject: this.i18n.__(`email.${templateCode}.subject`, variables),
      template_text: this.i18n.__(`email.${templateCode}.body`, variables),
      template_styles: this.i18n.__(`email.${templateCode}.styles`, { fallback: '' }),
      template_type: 2 // HTML
    };
  }

  /**
   * Generate Magento store configuration translation
   */
  generateStoreConfigTranslation(locale, storeId, configPaths) {
    this.i18n.setLocale(locale);
    const magentoLocale = this.mapI18nLocaleToMagento(locale);

    const configs = configPaths.map(path => ({
      path: path,
      scope: 'stores',
      scope_id: storeId,
      value: this.i18n.__(`config.${path.replace(/\//g, '.')}`)
    }));

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'translation',
        system: 'Magento Store Config',
        locale,
        storeId,
        configCount: configs.length
      });
    }

    return configs;
  }

  /**
   * Generate Magento REST API translation payload
   */
  generateRESTAPITranslation(entityType, entityId, locale, storeId, fields) {
    this.i18n.setLocale(locale);
    const magentoLocale = this.mapI18nLocaleToMagento(locale);

    const translatedFields = {};

    fields.forEach(field => {
      const translationKey = `${entityType}.${entityId}.${field}`;
      translatedFields[field] = this.i18n.__(translationKey);
    });

    return {
      entity_type: entityType,
      entity_id: entityId,
      store_id: storeId,
      locale: magentoLocale,
      translations: translatedFields
    };
  }

  /**
   * Export to Magento module i18n structure
   */
  exportToMagentoModule(locale, moduleName, vendorName = 'Custom') {
    const csv = this.generateMagentoCSV(locale, moduleName);
    const magentoLocale = this.mapI18nLocaleToMagento(locale);

    return {
      vendor: vendorName,
      module: moduleName,
      locale: magentoLocale,
      path: `app/code/${vendorName}/${moduleName}/i18n/${magentoLocale}.csv`,
      content: csv
    };
  }

  /**
   * Import translations from Magento CSV
   */
  async importFromMagentoCSV(csvContent, locale) {
    const updates = {};
    const lines = csvContent.split('\n');

    lines.forEach(line => {
      if (!line.trim()) return;

      // Parse CSV line (handle quoted fields)
      const matches = line.match(/"([^"]*)","([^"]*)","([^"]*)"/);

      if (matches) {
        const originalString = matches[1];
        const translatedString = matches[2];
        const module = matches[3];

        updates[originalString] = translatedString;
      }
    });

    if (this.audit) {
      this.audit.logCatalogModification({
        locale,
        operation: 'import',
        source: 'Magento CSV',
        keysUpdated: Object.keys(updates).length
      });
    }

    return {
      locale,
      imported: Object.keys(updates).length,
      updates
    };
  }

  /**
   * Generate Magento category translation
   */
  async translateCategory(category, locale, storeId) {
    this.i18n.setLocale(locale);
    const magentoLocale = this.mapI18nLocaleToMagento(locale);

    return {
      entity_id: category.id,
      store_id: storeId,
      locale: magentoLocale,
      name: this.i18n.__(`categories.${category.url_key}.name`, {
        fallback: category.name
      }),
      description: this.i18n.__(`categories.${category.url_key}.description`, {
        fallback: category.description
      }),
      meta_title: this.i18n.__(`categories.${category.url_key}.meta.title`, {
        fallback: category.meta_title
      }),
      meta_description: this.i18n.__(`categories.${category.url_key}.meta.description`, {
        fallback: category.meta_description
      }),
      meta_keywords: this.i18n__(`categories.${category.url_key}.meta.keywords`, {
        fallback: category.meta_keywords
      })
    };
  }

  /**
   * Express middleware for Magento integration
   */
  magentoMiddleware() {
    return (req, res, next) => {
      // Detect locale from Magento store view
      const magentoLocale = req.query.___store ||
                           req.headers['x-magento-locale'] ||
                           this.config.defaultLocale;

      const locale = this.mapMagentoLocaleToI18n(magentoLocale);
      this.i18n.setLocale(req, locale);

      // Add Magento-specific helpers
      req.magentoLocale = magentoLocale;
      req.translateAttribute = (attribute, storeId) =>
        this.translateProductAttribute(attribute, locale, storeId);
      req.translateCMSBlock = (block, storeId) =>
        this.translateCMSBlock(block, locale, storeId);

      res.locals.magentoLocale = magentoLocale;
      res.locals.i18nLocale = locale;

      next();
    };
  }

  /**
   * Generate Magento GraphQL translation response
   */
  generateGraphQLTranslation(type, data, locale) {
    this.i18n.setLocale(locale);

    const translate = (obj, typeName) => {
      const translated = { ...obj };

      Object.keys(translated).forEach(key => {
        if (typeof translated[key] === 'string') {
          const translationKey = `${typeName}.${key}`;
          translated[key] = this.i18n.__(translationKey, {
            fallback: translated[key]
          });
        }
      });

      return translated;
    };

    return translate(data, type);
  }

  /**
   * Escape CSV values
   */
  escapeCSV(value) {
    if (value === null || value === undefined) return '';
    return String(value).replace(/"/g, '""');
  }

  /**
   * Validate Magento payload
   */
  validateMagentoPayload(payload) {
    const errors = [];

    if (!payload.store_id && payload.store_id !== 0) {
      errors.push('Missing store_id');
    }

    if (!payload.locale) {
      errors.push('Missing locale');
    }

    return {
      valid: errors.length === 0,
      errors
    };
  }
}

module.exports = { MagentoI18nAdapter };

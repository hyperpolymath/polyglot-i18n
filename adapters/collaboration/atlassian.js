/**
 * Atlassian Integration Adapter
 * Supports: JIRA, Confluence, Bitbucket
 */

const { I18n } = require('../../index');
const { I18nAuditSystem } = require('../../audit/forensics');

class AtlassianI18nAdapter {
  constructor(config = {}) {
    this.config = {
      jiraHost: config.jiraHost || process.env.JIRA_HOST,
      confluenceHost: config.confluenceHost || process.env.CONFLUENCE_HOST,
      bitbucketHost: config.bitbucketHost || process.env.BITBUCKET_HOST,
      username: config.username || process.env.ATLASSIAN_USERNAME,
      apiToken: config.apiToken || process.env.ATLASSIAN_API_TOKEN,
      locales: config.locales || ['en-US', 'de-DE', 'fr-FR', 'es-ES', 'ja-JP'],
      auditEnabled: config.auditEnabled !== false,
      ...config
    };

    this.i18n = new I18n(config.i18n || {});

    if (this.config.auditEnabled) {
      this.audit = new I18nAuditSystem({
        enabled: true,
        logDir: config.auditLogDir || './audit-logs/atlassian'
      });
    }

    // Atlassian locale mappings
    this.localeMapping = {
      'en-US': 'en_US', 'en-GB': 'en_GB', 'de-DE': 'de_DE', 'fr-FR': 'fr_FR',
      'es-ES': 'es_ES', 'pt-BR': 'pt_BR', 'it-IT': 'it_IT', 'ja-JP': 'ja_JP',
      'zh-CN': 'zh_CN', 'ko-KR': 'ko_KR', 'nl-NL': 'nl_NL', 'pl-PL': 'pl_PL',
      'ru-RU': 'ru_RU', 'sv-SE': 'sv_SE', 'da-DK': 'da_DK', 'fi-FI': 'fi_FI'
    };
  }

  /**
   * Map Atlassian locale to i18n locale
   */
  mapAtlassianLocaleToI18n(atlassianLocale) {
    const reverseMapping = Object.entries(this.localeMapping)
      .reduce((acc, [i18nLocale, atlassianFormat]) => {
        acc[atlassianFormat] = i18nLocale;
        return acc;
      }, {});

    return reverseMapping[atlassianLocale] || 'en-US';
  }

  /**
   * Map i18n locale to Atlassian format
   */
  mapI18nLocaleToAtlassian(locale) {
    return this.localeMapping[locale] || 'en_US';
  }

  /**
   * Generate JIRA i18n properties file
   */
  generateJiraPropertiesFile(locale) {
    const catalog = this.i18n.getCatalog(locale);
    const atlassianLocale = this.mapI18nLocaleToAtlassian(locale);

    let properties = `# JIRA i18n Properties File\n`;
    properties += `# Locale: ${atlassianLocale}\n`;
    properties += `# Generated: ${new Date().toISOString()}\n\n`;

    const flatten = (obj, prefix = '') => {
      Object.entries(obj).forEach(([key, value]) => {
        const fullKey = prefix ? `${prefix}.${key}` : key;

        if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
          flatten(value, fullKey);
        } else {
          // Escape special characters for Java properties format
          const escapedValue = String(value)
            .replace(/\\/g, '\\\\')
            .replace(/\n/g, '\\n')
            .replace(/\r/g, '\\r')
            .replace(/\t/g, '\\t')
            .replace(/=/g, '\\=')
            .replace(/:/g, '\\:');

          properties += `${fullKey}=${escapedValue}\n`;
        }
      });
    };

    flatten(catalog);

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'export',
        system: 'JIRA',
        locale,
        format: 'properties',
        atlassianLocale
      });
    }

    return properties;
  }

  /**
   * Generate Confluence storage format with translations
   */
  generateConfluenceStorageFormat(pageTitle, locale, contentKeys = []) {
    const atlassianLocale = this.mapI18nLocaleToAtlassian(locale);
    this.i18n.setLocale(locale);

    let storage = `<ac:structured-macro ac:name="info">\n`;
    storage += `  <ac:parameter ac:name="title">Translations (${locale})</ac:parameter>\n`;
    storage += `  <ac:rich-text-body>\n`;
    storage += `    <table>\n`;
    storage += `      <tbody>\n`;
    storage += `        <tr>\n`;
    storage += `          <th>Key</th>\n`;
    storage += `          <th>Translation</th>\n`;
    storage += `        </tr>\n`;

    contentKeys.forEach(key => {
      const translation = this.i18n.__(key);
      storage += `        <tr>\n`;
      storage += `          <td>${this.escapeConfluenceXML(key)}</td>\n`;
      storage += `          <td>${this.escapeConfluenceXML(translation)}</td>\n`;
      storage += `        </tr>\n`;
    });

    storage += `      </tbody>\n`;
    storage += `    </table>\n`;
    storage += `  </ac:rich-text-body>\n`;
    storage += `</ac:structured-macro>\n`;

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'export',
        system: 'Confluence',
        locale,
        format: 'storage',
        pageTitle,
        keyCount: contentKeys.length
      });
    }

    return storage;
  }

  /**
   * Generate Bitbucket pipeline localization file
   */
  generateBitbucketPipelineI18n(locale) {
    const catalog = this.i18n.getCatalog(locale);

    const pipelineI18n = {
      locale: locale,
      messages: {}
    };

    const flatten = (obj, prefix = '') => {
      Object.entries(obj).forEach(([key, value]) => {
        const fullKey = prefix ? `${prefix}.${key}` : key;

        if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
          flatten(value, fullKey);
        } else {
          pipelineI18n.messages[fullKey] = String(value);
        }
      });
    };

    flatten(catalog);

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'export',
        system: 'Bitbucket',
        locale,
        format: 'JSON',
        messageCount: Object.keys(pipelineI18n.messages).length
      });
    }

    return pipelineI18n;
  }

  /**
   * Translate JIRA issue field
   */
  async translateJiraField(fieldKey, locale, params = {}) {
    const targetLocale = this.mapAtlassianLocaleToI18n(locale);
    this.i18n.setLocale(targetLocale);

    const translated = this.i18n.__(fieldKey, params);

    if (this.audit) {
      this.audit.logTranslation({
        system: 'JIRA',
        operation: 'translateJiraField',
        fieldKey,
        locale: targetLocale,
        atlassianLocale: locale,
        result: translated
      });
    }

    return translated;
  }

  /**
   * Generate JIRA custom field translations
   */
  generateJiraCustomFieldTranslations(locale, customFields) {
    const atlassianLocale = this.mapI18nLocaleToAtlassian(locale);
    this.i18n.setLocale(locale);

    const translations = customFields.map(field => ({
      customFieldId: field.id,
      name: this.i18n.__(field.nameKey || field.id),
      description: this.i18n.__(field.descriptionKey || `${field.id}.description`),
      locale: atlassianLocale,
      options: field.options ? field.options.map(opt => ({
        value: opt.value,
        label: this.i18n.__(opt.labelKey || opt.value)
      })) : []
    }));

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'translation',
        system: 'JIRA Custom Fields',
        locale,
        fieldCount: customFields.length
      });
    }

    return translations;
  }

  /**
   * Generate Confluence macro translations
   */
  generateConfluenceMacroTranslations(locale, macroKeys) {
    this.i18n.setLocale(locale);

    const macroTranslations = {};

    macroKeys.forEach(key => {
      macroTranslations[key] = {
        title: this.i18n.__(`${key}.title`),
        description: this.i18n.__(`${key}.description`),
        parameterDescriptions: {}
      };
    });

    return macroTranslations;
  }

  /**
   * Export to Atlassian Marketplace app i18n format
   */
  exportToMarketplaceFormat(locale, appKey) {
    const catalog = this.i18n.getCatalog(locale);
    const atlassianLocale = this.mapI18nLocaleToAtlassian(locale);

    const marketplaceI18n = {
      appKey: appKey,
      locale: atlassianLocale,
      translations: {}
    };

    const flatten = (obj, prefix = '') => {
      Object.entries(obj).forEach(([key, value]) => {
        const fullKey = prefix ? `${prefix}.${key}` : key;

        if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
          flatten(value, fullKey);
        } else {
          marketplaceI18n.translations[fullKey] = String(value);
        }
      });
    };

    flatten(catalog);

    return marketplaceI18n;
  }

  /**
   * Import translations from JIRA properties file
   */
  async importFromJiraProperties(propertiesContent, locale) {
    const updates = {};
    const lines = propertiesContent.split('\n');

    lines.forEach(line => {
      // Skip comments and empty lines
      if (line.trim().startsWith('#') || !line.trim()) return;

      const match = line.match(/^([^=:]+)=(.*)$/);
      if (match) {
        const key = match[1].trim();
        let value = match[2].trim();

        // Unescape Java properties format
        value = value
          .replace(/\\n/g, '\n')
          .replace(/\\r/g, '\r')
          .replace(/\\t/g, '\t')
          .replace(/\\=/g, '=')
          .replace(/\\:/g, ':')
          .replace(/\\\\/g, '\\');

        updates[key] = value;
      }
    });

    if (this.audit) {
      this.audit.logCatalogModification({
        locale,
        operation: 'import',
        source: 'JIRA Properties',
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
   * Express middleware for Atlassian Connect apps
   */
  atlassianConnectMiddleware() {
    return (req, res, next) => {
      const atlassianLocale = req.query.locale ||
                             req.headers['atlassian-locale'] ||
                             'en_US';

      const locale = this.mapAtlassianLocaleToI18n(atlassianLocale);
      this.i18n.setLocale(req, locale);

      // Add Atlassian-specific translation helpers
      req.translateJira = (fieldKey, params) => {
        return this.translateJiraField(fieldKey, atlassianLocale, params);
      };

      res.locals.atlassianLocale = atlassianLocale;
      res.locals.i18nLocale = locale;
      res.locals.jiraTranslate = req.translateJira;

      next();
    };
  }

  /**
   * Generate JIRA workflow translation
   */
  generateJiraWorkflowTranslations(locale, workflow) {
    this.i18n.setLocale(locale);

    return {
      workflowName: workflow.name,
      locale: this.mapI18nLocaleToAtlassian(locale),
      steps: workflow.steps.map(step => ({
        id: step.id,
        name: this.i18n.__(`workflow.${workflow.name}.step.${step.id}.name`),
        description: this.i18n.__(`workflow.${workflow.name}.step.${step.id}.description`)
      })),
      transitions: workflow.transitions.map(trans => ({
        id: trans.id,
        name: this.i18n.__(`workflow.${workflow.name}.transition.${trans.id}.name`),
        description: this.i18n.__(`workflow.${workflow.name}.transition.${trans.id}.description`)
      }))
    };
  }

  /**
   * Escape XML for Confluence storage format
   */
  escapeConfluenceXML(str) {
    if (!str) return '';
    return String(str)
      .replace(/&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
      .replace(/"/g, '&quot;');
  }

  /**
   * Validate Atlassian payload
   */
  validateAtlassianPayload(payload, product) {
    const errors = [];

    if (!payload.locale) {
      errors.push('Missing locale');
    }

    if (product === 'jira' && !payload.translations && !payload.messages) {
      errors.push('Missing translations or messages object');
    }

    if (product === 'confluence' && !payload.content) {
      errors.push('Missing content for Confluence');
    }

    return {
      valid: errors.length === 0,
      errors
    };
  }
}

module.exports = { AtlassianI18nAdapter };

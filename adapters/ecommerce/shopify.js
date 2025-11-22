/**
 * Shopify Integration Adapter
 * Supports: Product translations, Theme localization, Checkout translations
 */

const { I18n } = require('../../index');
const { I18nAuditSystem } = require('../../audit/forensics');

class ShopifyI18nAdapter {
  constructor(config = {}) {
    this.config = {
      shopDomain: config.shopDomain || process.env.SHOPIFY_SHOP_DOMAIN,
      accessToken: config.accessToken || process.env.SHOPIFY_ACCESS_TOKEN,
      apiVersion: config.apiVersion || '2024-01',
      locales: config.locales || ['en', 'de', 'fr', 'es', 'ja'],
      defaultLocale: config.defaultLocale || 'en',
      auditEnabled: config.auditEnabled !== false,
      ...config
    };

    this.i18n = new I18n(config.i18n || {});

    if (this.config.auditEnabled) {
      this.audit = new I18nAuditSystem({
        enabled: true,
        logDir: config.auditLogDir || './audit-logs/shopify'
      });
    }

    // Shopify locale format mappings
    this.localeMapping = {
      'en-US': 'en', 'en-GB': 'en-GB', 'de-DE': 'de', 'fr-FR': 'fr',
      'es-ES': 'es', 'pt-BR': 'pt-BR', 'it-IT': 'it', 'ja-JP': 'ja',
      'zh-CN': 'zh-CN', 'zh-TW': 'zh-TW', 'ko-KR': 'ko', 'nl-NL': 'nl',
      'pl-PL': 'pl', 'ru-RU': 'ru', 'sv-SE': 'sv', 'da-DK': 'da'
    };
  }

  /**
   * Map Shopify locale to i18n locale
   */
  mapShopifyLocaleToI18n(shopifyLocale) {
    const reverseMapping = Object.entries(this.localeMapping)
      .reduce((acc, [i18nLocale, shopifyFormat]) => {
        acc[shopifyFormat] = i18nLocale;
        return acc;
      }, {});

    return reverseMapping[shopifyLocale] || this.config.defaultLocale;
  }

  /**
   * Map i18n locale to Shopify format
   */
  mapI18nLocaleToShopify(locale) {
    return this.localeMapping[locale] || 'en';
  }

  /**
   * Generate Shopify theme locale JSON file
   */
  generateThemeLocaleFile(locale) {
    const catalog = this.i18n.getCatalog(locale);
    const shopifyLocale = this.mapI18nLocaleToShopify(locale);

    const themeLocale = {
      general: {},
      products: {},
      cart: {},
      checkout: {},
      customer: {},
      sections: {},
      templates: {}
    };

    // Organize translations by Shopify theme structure
    Object.entries(catalog).forEach(([key, value]) => {
      const parts = key.split('.');

      if (parts[0] === 'general') {
        this.setNestedValue(themeLocale.general, parts.slice(1), value);
      } else if (parts[0] === 'products') {
        this.setNestedValue(themeLocale.products, parts.slice(1), value);
      } else if (parts[0] === 'cart') {
        this.setNestedValue(themeLocale.cart, parts.slice(1), value);
      } else if (parts[0] === 'checkout') {
        this.setNestedValue(themeLocale.checkout, parts.slice(1), value);
      } else if (parts[0] === 'customer') {
        this.setNestedValue(themeLocale.customer, parts.slice(1), value);
      } else if (parts[0] === 'sections') {
        this.setNestedValue(themeLocale.sections, parts.slice(1), value);
      } else if (parts[0] === 'templates') {
        this.setNestedValue(themeLocale.templates, parts.slice(1), value);
      } else {
        themeLocale[key] = value;
      }
    });

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'export',
        system: 'Shopify Theme',
        locale,
        shopifyLocale,
        format: 'JSON'
      });
    }

    return themeLocale;
  }

  /**
   * Generate Shopify Admin API translation payload
   */
  generateAdminAPITranslation(resourceType, resourceId, locale, translationKeys) {
    const shopifyLocale = this.mapI18nLocaleToShopify(locale);
    this.i18n.setLocale(locale);

    const translations = [];

    translationKeys.forEach(keyConfig => {
      translations.push({
        locale: shopifyLocale,
        key: keyConfig.key,
        value: this.i18n.__(keyConfig.translationKey || keyConfig.key),
        translatable_content_digest: keyConfig.digest
      });
    });

    const payload = {
      translation: {
        locale: shopifyLocale,
        translatable_id: resourceId,
        translatable_type: resourceType,
        translations: translations
      }
    };

    if (this.audit) {
      this.audit.logTranslation({
        system: 'Shopify Admin API',
        operation: 'generateAdminAPITranslation',
        resourceType,
        resourceId,
        locale,
        translationCount: translations.length
      });
    }

    return payload;
  }

  /**
   * Translate Shopify product
   */
  async translateProduct(product, locale) {
    this.i18n.setLocale(locale);
    const shopifyLocale = this.mapI18nLocaleToShopify(locale);

    const translatedProduct = {
      id: product.id,
      locale: shopifyLocale,
      title: this.i18n.__(`products.${product.handle}.title`, { fallback: product.title }),
      body_html: this.i18n__(`products.${product.handle}.description`, { fallback: product.body_html }),
      vendor: product.vendor,
      product_type: this.i18n.__(`product_types.${product.product_type}`, { fallback: product.product_type }),
      tags: product.tags,
      variants: product.variants ? product.variants.map(variant => ({
        id: variant.id,
        title: this.i18n.__(`products.${product.handle}.variants.${variant.id}.title`, { fallback: variant.title }),
        option1: variant.option1 ? this.i18n.__(`products.options.${variant.option1}`, { fallback: variant.option1 }) : null,
        option2: variant.option2 ? this.i18n__(`products.options.${variant.option2}`, { fallback: variant.option2 }) : null,
        option3: variant.option3 ? this.i18n__(`products.options.${variant.option3}`, { fallback: variant.option3 }) : null,
        price: variant.price,
        sku: variant.sku
      })) : [],
      metafields: product.metafields ? product.metafields.map(mf => ({
        namespace: mf.namespace,
        key: mf.key,
        value: this.i18n.__(`products.${product.handle}.metafields.${mf.namespace}.${mf.key}`, { fallback: mf.value })
      })) : []
    };

    if (this.audit) {
      this.audit.logTranslation({
        system: 'Shopify Product',
        operation: 'translateProduct',
        productId: product.id,
        productHandle: product.handle,
        locale
      });
    }

    return translatedProduct;
  }

  /**
   * Generate Shopify collection translation
   */
  async translateCollection(collection, locale) {
    this.i18n.setLocale(locale);
    const shopifyLocale = this.mapI18nLocaleToShopify(locale);

    return {
      id: collection.id,
      locale: shopifyLocale,
      title: this.i18n.__(`collections.${collection.handle}.title`, { fallback: collection.title }),
      body_html: this.i18n.__(`collections.${collection.handle}.description`, { fallback: collection.body_html }),
      handle: collection.handle,
      seo_title: this.i18n.__(`collections.${collection.handle}.seo.title`, { fallback: collection.title }),
      seo_description: this.i18n__(`collections.${collection.handle}.seo.description`, { fallback: '' })
    };
  }

  /**
   * Generate Shopify metaobject translation
   */
  generateMetaobjectTranslation(metaobject, locale) {
    this.i18n.setLocale(locale);
    const shopifyLocale = this.mapI18nLocaleToShopify(locale);

    const translatedFields = {};

    Object.entries(metaobject.fields).forEach(([key, value]) => {
      translatedFields[key] = this.i18n.__(`metaobjects.${metaobject.type}.${key}`, { fallback: value });
    });

    return {
      id: metaobject.id,
      type: metaobject.type,
      locale: shopifyLocale,
      fields: translatedFields
    };
  }

  /**
   * Generate Shopify shop policy translation
   */
  generatePolicyTranslation(policyType, locale) {
    this.i18n.setLocale(locale);
    const shopifyLocale = this.mapI18nLocaleToShopify(locale);

    return {
      locale: shopifyLocale,
      type: policyType,
      title: this.i18n.__(`policies.${policyType}.title`),
      body: this.i18n.__(`policies.${policyType}.body`),
      url: `/${shopifyLocale}/policies/${policyType}`
    };
  }

  /**
   * Export catalog to Shopify theme locales structure
   */
  exportToShopifyTheme(locale, themeName) {
    const themeLocaleContent = this.generateThemeLocaleFile(locale);
    const shopifyLocale = this.mapI18nLocaleToShopify(locale);

    const themeExport = {
      themeName: themeName,
      locale: shopifyLocale,
      path: `locales/${shopifyLocale}.json`,
      content: JSON.stringify(themeLocaleContent, null, 2)
    };

    return themeExport;
  }

  /**
   * Import translations from Shopify theme locale file
   */
  async importFromShopifyTheme(themeLocaleContent, locale) {
    const updates = {};

    const flatten = (obj, prefix = '') => {
      Object.entries(obj).forEach(([key, value]) => {
        const fullKey = prefix ? `${prefix}.${key}` : key;

        if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
          flatten(value, fullKey);
        } else {
          updates[fullKey] = value;
        }
      });
    };

    flatten(themeLocaleContent);

    if (this.audit) {
      this.audit.logCatalogModification({
        locale,
        operation: 'import',
        source: 'Shopify Theme',
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
   * Translate Shopify notification email template
   */
  translateEmailTemplate(templateKey, locale, variables = {}) {
    this.i18n.setLocale(locale);

    return {
      locale: this.mapI18nLocaleToShopify(locale),
      subject: this.i18n.__(`email.${templateKey}.subject`, variables),
      body: this.i18n.__(`email.${templateKey}.body`, variables),
      preheader: this.i18n.__(`email.${templateKey}.preheader`, variables)
    };
  }

  /**
   * Express middleware for Shopify apps
   */
  shopifyMiddleware() {
    return (req, res, next) => {
      // Detect locale from Shopify request
      const shopifyLocale = req.query.locale ||
                           req.headers['x-shopify-locale'] ||
                           this.config.defaultLocale;

      const locale = this.mapShopifyLocaleToI18n(shopifyLocale);
      this.i18n.setLocale(req, locale);

      // Add Shopify-specific helpers
      req.shopifyLocale = shopifyLocale;
      req.translateProduct = (product) => this.translateProduct(product, locale);
      req.translateCollection = (collection) => this.translateCollection(collection, locale);

      res.locals.shopifyLocale = shopifyLocale;
      res.locals.i18nLocale = locale;

      next();
    };
  }

  /**
   * Generate Shopify App Bridge translation payload
   */
  generateAppBridgeTranslations(locale, appSections) {
    this.i18n.setLocale(locale);

    const translations = {};

    appSections.forEach(section => {
      translations[section] = {};

      const sectionCatalog = this.i18n.getCatalog(locale);
      Object.entries(sectionCatalog).forEach(([key, value]) => {
        if (key.startsWith(`${section}.`)) {
          const sectionKey = key.substring(section.length + 1);
          translations[section][sectionKey] = value;
        }
      });
    });

    return {
      locale: this.mapI18nLocaleToShopify(locale),
      translations
    };
  }

  /**
   * Set nested value in object using path array
   */
  setNestedValue(obj, path, value) {
    if (path.length === 0) return;
    if (path.length === 1) {
      obj[path[0]] = value;
      return;
    }

    const key = path[0];
    if (!obj[key] || typeof obj[key] !== 'object') {
      obj[key] = {};
    }

    this.setNestedValue(obj[key], path.slice(1), value);
  }

  /**
   * Validate Shopify translation payload
   */
  validateShopifyPayload(payload) {
    const errors = [];

    if (!payload.locale) {
      errors.push('Missing locale');
    }

    if (payload.translation) {
      if (!payload.translation.translatable_id) {
        errors.push('Missing translatable_id');
      }
      if (!payload.translation.translatable_type) {
        errors.push('Missing translatable_type');
      }
    }

    return {
      valid: errors.length === 0,
      errors
    };
  }
}

module.exports = { ShopifyI18nAdapter };

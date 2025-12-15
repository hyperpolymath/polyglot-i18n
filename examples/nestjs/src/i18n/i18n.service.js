/**
 * I18n NestJS Service (JavaScript)
 */

const { Injectable, Inject, Scope } = require('@nestjs/common');
const { I18n } = require('i18n');

// Decorators applied via Reflect.decorate() below
class I18nService {
  constructor(options, request) {
    this.i18n = new I18n(options);
    this.request = request;

    // Detect locale from request
    const locale = this.detectLocale(request, options);
    this.i18n.setLocale(locale);
    this.currentLocale = locale;
  }

  detectLocale(req, options) {
    // Priority: query param > header > cookie > default
    return (
      req.query?.locale ||
      req.query?.lang ||
      req.headers['accept-language']?.split(',')[0]?.split('-')[0] ||
      req.cookies?.locale ||
      options.defaultLocale ||
      'en'
    );
  }

  __(phrase, ...args) {
    return this.i18n.__(phrase, ...args);
  }

  __n(singular, plural, count, ...args) {
    return this.i18n.__n(singular, plural, count, ...args);
  }

  __mf(phrase, ...args) {
    return this.i18n.__mf(phrase, ...args);
  }

  __l(phrase) {
    return this.i18n.__l(phrase);
  }

  __h(phrase) {
    return this.i18n.__h(phrase);
  }

  setLocale(locale) {
    this.i18n.setLocale(locale);
    this.currentLocale = locale;
    return locale;
  }

  getLocale() {
    return this.currentLocale || this.i18n.getLocale();
  }

  getLocales() {
    return this.i18n.getLocales();
  }

  getCatalog(locale) {
    return this.i18n.getCatalog(locale);
  }

  translate(phrase, params = {}) {
    if (typeof params === 'object' && params !== null) {
      return this.i18n.__(phrase, params);
    }
    return this.i18n.__(phrase);
  }

  translatePlural(singular, plural, count, params = {}) {
    return this.i18n.__n(singular, plural, count, params);
  }
}

// Apply NestJS decorator
Reflect.decorate(
  [Injectable({ scope: Scope.REQUEST })],
  I18nService
);

module.exports = { I18nService };

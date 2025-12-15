/**
 * Greetings Controller (JavaScript)
 * Demonstrates i18n usage in NestJS controllers
 */

const { Controller, Get, Param, Query } = require('@nestjs/common');
const { I18nService } = require('../i18n/i18n.service');

// Decorators applied via Reflect.decorate() below
class GreetingsController {
  constructor(i18nService) {
    this.i18nService = i18nService;
  }

  async getGreeting(name) {
    return {
      message: this.i18nService.__('greeting', name || 'World'),
      locale: this.i18nService.getLocale()
    };
  }

  async getAvailableLocales() {
    return {
      locales: this.i18nService.getLocales(),
      current: this.i18nService.getLocale()
    };
  }

  async getCatalog() {
    return {
      catalog: this.i18nService.getCatalog(),
      locale: this.i18nService.getLocale()
    };
  }

  async getGreetingInLocale(locale, name) {
    this.i18nService.setLocale(locale);

    return {
      message: this.i18nService.__('greeting', name || 'World'),
      locale: this.i18nService.getLocale()
    };
  }

  async getPluralGreeting(count) {
    const num = parseInt(count, 10);

    return {
      message: this.i18nService.__n('%s item', '%s items', num),
      count: num,
      locale: this.i18nService.getLocale()
    };
  }
}

// Apply NestJS decorators
Reflect.decorate(
  [Controller('greetings')],
  GreetingsController
);

// Method decorators
Reflect.decorate(
  [Get()],
  GreetingsController.prototype,
  'getGreeting'
);

Reflect.decorate(
  [Get('locales')],
  GreetingsController.prototype,
  'getAvailableLocales'
);

Reflect.decorate(
  [Get('catalog')],
  GreetingsController.prototype,
  'getCatalog'
);

Reflect.decorate(
  [Get(':locale')],
  GreetingsController.prototype,
  'getGreetingInLocale'
);

Reflect.decorate(
  [Get('plural/:count')],
  GreetingsController.prototype,
  'getPluralGreeting'
);

module.exports = { GreetingsController };

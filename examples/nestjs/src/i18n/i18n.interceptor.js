/**
 * I18n NestJS Interceptor (JavaScript)
 * Automatically adds locale information to responses
 */

const { Injectable, NestInterceptor, ExecutionContext, CallHandler } = require('@nestjs/common');
const { map } = require('rxjs/operators');

// Decorators applied via Reflect.decorate() below
class I18nInterceptor {
  constructor(i18nService) {
    this.i18nService = i18nService;
  }

  intercept(context, next) {
    const request = context.switchToHttp().getRequest();
    const locale = request.i18nService?.getLocale() || 'en';

    return next.handle().pipe(
      map(data => {
        // Add locale metadata to response
        if (typeof data === 'object' && data !== null) {
          return {
            ...data,
            _meta: {
              locale,
              ...(data._meta || {})
            }
          };
        }
        return data;
      })
    );
  }
}

// Apply NestJS decorator
Reflect.decorate(
  [Injectable()],
  I18nInterceptor
);

module.exports = { I18nInterceptor };

/**
 * Comprehensive Observability and Telemetry for i18n
 * OpenTelemetry, Prometheus, Grafana, Datadog, New Relic integration
 */

const { I18n } = require('../index');

class I18nObservability {
  constructor(config = {}) {
    this.config = {
      enabled: config.enabled !== false,
      provider: config.provider || 'opentelemetry', // 'opentelemetry', 'prometheus', 'datadog', 'newrelic'
      exportInterval: config.exportInterval || 60000, // 1 minute
      ...config
    };

    this.metrics = {
      translations: {
        total: 0,
        byLocale: {},
        byKey: {},
        errors: 0,
        cacheHits: 0,
        cacheMisses: 0
      },
      performance: {
        avgDuration: 0,
        p50: 0,
        p95: 0,
        p99: 0,
        samples: []
      },
      localeChanges: 0,
      catalogUpdates: 0
    };

    if (this.config.enabled) {
      this.initializeTelemetry();
    }
  }

  initializeTelemetry() {
    switch (this.config.provider) {
      case 'opentelemetry':
        this.initializeOpenTelemetry();
        break;
      case 'prometheus':
        this.initializePrometheus();
        break;
      case 'datadog':
        this.initializeDatadog();
        break;
      case 'newrelic':
        this.initializeNewRelic();
        break;
    }

    // Start export interval
    this.exportInterval = setInterval(() => this.export(), this.config.exportInterval);
  }

  initializeOpenTelemetry() {
    // OpenTelemetry setup
    try {
      const { MeterProvider, PeriodicExportingMetricReader } = require('@opentelemetry/sdk-metrics');
      const { Resource } = require('@opentelemetry/resources');
      const { SemanticResourceAttributes } = require('@opentelemetry/semantic-conventions');

      const resource = Resource.default().merge(
        new Resource({
          [SemanticResourceAttributes.SERVICE_NAME]: 'i18n',
          [SemanticResourceAttributes.SERVICE_VERSION]: require('../package.json').version
        })
      );

      this.meterProvider = new MeterProvider({ resource });
      this.meter = this.meterProvider.getMeter('i18n');

      // Create metrics
      this.otelMetrics = {
        translationCounter: this.meter.createCounter('i18n.translations.total', {
          description: 'Total number of translations'
        }),
        translationDuration: this.meter.createHistogram('i18n.translation.duration', {
          description: 'Translation operation duration',
          unit: 'ms'
        }),
        cacheHitRatio: this.meter.createObservableGauge('i18n.cache.hit_ratio', {
          description: 'Cache hit ratio'
        }),
        activeLocales: this.meter.createObservableGauge('i18n.locales.active', {
          description: 'Number of active locales'
        })
      };
    } catch (err) {
      console.warn('OpenTelemetry not available:', err.message);
    }
  }

  initializePrometheus() {
    // Prometheus setup
    try {
      const promClient = require('prom-client');

      this.promRegistry = new promClient.Registry();

      this.promMetrics = {
        translationCounter: new promClient.Counter({
          name: 'i18n_translations_total',
          help: 'Total number of translations',
          labelNames: ['locale', 'operation'],
          registers: [this.promRegistry]
        }),
        translationDuration: new promClient.Histogram({
          name: 'i18n_translation_duration_ms',
          help: 'Translation duration in milliseconds',
          labelNames: ['locale', 'operation'],
          buckets: [1, 5, 10, 25, 50, 100, 250, 500],
          registers: [this.promRegistry]
        }),
        cacheHitRatio: new promClient.Gauge({
          name: 'i18n_cache_hit_ratio',
          help: 'Cache hit ratio (0-1)',
          registers: [this.promRegistry]
        }),
        catalogSize: new promClient.Gauge({
          name: 'i18n_catalog_size',
          help: 'Number of translation keys',
          labelNames: ['locale'],
          registers: [this.promRegistry]
        })
      };

      // Collect default metrics
      promClient.collectDefaultMetrics({ register: this.promRegistry });
    } catch (err) {
      console.warn('Prometheus client not available:', err.message);
    }
  }

  initializeDatadog() {
    // Datadog StatsD setup
    try {
      const StatsD = require('hot-shots');

      this.statsd = new StatsD({
        host: this.config.datadogHost || 'localhost',
        port: this.config.datadogPort || 8125,
        prefix: 'i18n.',
        globalTags: {
          service: 'i18n',
          version: require('../package.json').version
        }
      });
    } catch (err) {
      console.warn('Datadog StatsD not available:', err.message);
    }
  }

  initializeNewRelic() {
    // New Relic setup
    try {
      this.newrelic = require('newrelic');
    } catch (err) {
      console.warn('New Relic not available:', err.message);
    }
  }

  /**
   * Record translation operation
   */
  recordTranslation(operation, locale, key, duration, cached = false) {
    this.metrics.translations.total++;
    this.metrics.translations.byLocale[locale] = (this.metrics.translations.byLocale[locale] || 0) + 1;
    this.metrics.translations.byKey[key] = (this.metrics.translations.byKey[key] || 0) + 1;

    if (cached) {
      this.metrics.translations.cacheHits++;
    } else {
      this.metrics.translations.cacheMisses++;
    }

    // Record duration
    this.metrics.performance.samples.push(duration);
    if (this.metrics.performance.samples.length > 1000) {
      this.metrics.performance.samples.shift();
    }

    // Update metrics based on provider
    if (this.otelMetrics) {
      this.otelMetrics.translationCounter.add(1, { locale, operation });
      this.otelMetrics.translationDuration.record(duration, { locale, operation });
    }

    if (this.promMetrics) {
      this.promMetrics.translationCounter.inc({ locale, operation });
      this.promMetrics.translationDuration.observe({ locale, operation }, duration);
    }

    if (this.statsd) {
      this.statsd.increment('translations.total', 1, { locale, operation });
      this.statsd.timing('translation.duration', duration, { locale, operation });
    }

    if (this.newrelic) {
      this.newrelic.recordMetric('Custom/i18n/Translation', duration);
      this.newrelic.incrementMetric(`Custom/i18n/Locale/${locale}`);
    }
  }

  /**
   * Record error
   */
  recordError(operation, locale, error) {
    this.metrics.translations.errors++;

    if (this.statsd) {
      this.statsd.increment('translations.errors', 1, { locale, operation });
    }

    if (this.newrelic) {
      this.newrelic.noticeError(error, { locale, operation });
    }
  }

  /**
   * Calculate performance percentiles
   */
  calculatePercentiles() {
    const sorted = [...this.metrics.performance.samples].sort((a, b) => a - b);
    const len = sorted.length;

    if (len === 0) return;

    this.metrics.performance.avgDuration = sorted.reduce((a, b) => a + b, 0) / len;
    this.metrics.performance.p50 = sorted[Math.floor(len * 0.5)];
    this.metrics.performance.p95 = sorted[Math.floor(len * 0.95)];
    this.metrics.performance.p99 = sorted[Math.floor(len * 0.99)];
  }

  /**
   * Export metrics
   */
  export() {
    this.calculatePercentiles();

    // Update gauge metrics
    if (this.promMetrics) {
      const hitRatio = this.metrics.translations.total > 0
        ? this.metrics.translations.cacheHits / this.metrics.translations.total
        : 0;

      this.promMetrics.cacheHitRatio.set(hitRatio);

      Object.entries(this.metrics.translations.byLocale).forEach(([locale, count]) => {
        this.promMetrics.catalogSize.set({ locale }, count);
      });
    }

    if (this.statsd) {
      const hitRatio = this.metrics.translations.total > 0
        ? (this.metrics.translations.cacheHits / this.metrics.translations.total) * 100
        : 0;

      this.statsd.gauge('cache.hit_ratio', hitRatio);
      this.statsd.gauge('performance.avg_duration', this.metrics.performance.avgDuration);
      this.statsd.gauge('performance.p95', this.metrics.performance.p95);
      this.statsd.gauge('performance.p99', this.metrics.performance.p99);
    }
  }

  /**
   * Get Prometheus metrics endpoint handler
   */
  getPrometheusHandler() {
    return async (req, res) => {
      res.setHeader('Content-Type', this.promRegistry.contentType);
      res.end(await this.promRegistry.metrics());
    };
  }

  /**
   * Get current metrics
   */
  getMetrics() {
    this.calculatePercentiles();
    return JSON.parse(JSON.stringify(this.metrics));
  }

  /**
   * Shutdown telemetry
   */
  shutdown() {
    if (this.exportInterval) {
      clearInterval(this.exportInterval);
    }

    if (this.statsd) {
      this.statsd.close();
    }

    this.export(); // Final export
  }
}

/**
 * Create instrumented i18n instance
 */
class InstrumentedI18n extends I18n {
  constructor(config = {}) {
    super(config);

    this.observability = new I18nObservability(config.observability || {});

    // Wrap translation methods
    this._originalTranslate = this.__;
    this.__ = this._instrumentedTranslate.bind(this);
  }

  _instrumentedTranslate(phrase, ...args) {
    const start = Date.now();
    const locale = this.getLocale();

    try {
      const result = this._originalTranslate.call(this, phrase, ...args);
      const duration = Date.now() - start;

      this.observability.recordTranslation('__', locale, phrase, duration, false);

      return result;
    } catch (error) {
      this.observability.recordError('__', locale, error);
      throw error;
    }
  }

  getMetrics() {
    return this.observability.getMetrics();
  }

  shutdown() {
    this.observability.shutdown();
  }
}

module.exports = { I18nObservability, InstrumentedI18n };

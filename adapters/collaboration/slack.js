/**
 * Slack Integration Adapter
 * Supports: Bot messages, Workspace localization, Slash commands, Interactive components
 */

const { I18n } = require('../../index');
const { I18nAuditSystem } = require('../../audit/forensics');

class SlackI18nAdapter {
  constructor(config = {}) {
    this.config = {
      botToken: config.botToken || process.env.SLACK_BOT_TOKEN,
      signingSecret: config.signingSecret || process.env.SLACK_SIGNING_SECRET,
      appToken: config.appToken || process.env.SLACK_APP_TOKEN,
      locales: config.locales || ['en-US', 'ja-JP', 'es-ES', 'de-DE', 'fr-FR'],
      defaultLocale: config.defaultLocale || 'en-US',
      auditEnabled: config.auditEnabled !== false,
      ...config
    };

    this.i18n = new I18n(config.i18n || {});

    if (this.config.auditEnabled) {
      this.audit = new I18nAuditSystem({
        enabled: true,
        logDir: config.auditLogDir || './audit-logs/slack'
      });
    }

    // Slack locale mappings (simplified locale codes)
    this.localeMapping = {
      'en-US': 'en', 'en-GB': 'en-GB', 'de-DE': 'de', 'fr-FR': 'fr',
      'es-ES': 'es', 'es-MX': 'es-MX', 'pt-BR': 'pt-BR', 'it-IT': 'it',
      'ja-JP': 'ja', 'zh-CN': 'zh-CN', 'zh-TW': 'zh-TW', 'ko-KR': 'ko',
      'nl-NL': 'nl', 'ru-RU': 'ru', 'pl-PL': 'pl', 'sv-SE': 'sv'
    };
  }

  /**
   * Map Slack locale to i18n locale
   */
  mapSlackLocaleToI18n(slackLocale) {
    const reverseMapping = Object.entries(this.localeMapping)
      .reduce((acc, [i18nLocale, slackFormat]) => {
        acc[slackFormat] = i18nLocale;
        return acc;
      }, {});

    return reverseMapping[slackLocale] || this.config.defaultLocale;
  }

  /**
   * Map i18n locale to Slack format
   */
  mapI18nLocaleToSlack(locale) {
    return this.localeMapping[locale] || 'en';
  }

  /**
   * Detect user's locale from Slack user object
   */
  detectUserLocale(slackUser) {
    // Slack provides locale in user object
    if (slackUser && slackUser.locale) {
      return this.mapSlackLocaleToI18n(slackUser.locale);
    }

    // Fallback to team locale if available
    if (slackUser && slackUser.tz) {
      // Simple timezone to locale mapping
      const tzLocaleMapping = {
        'America/New_York': 'en-US',
        'America/Los_Angeles': 'en-US',
        'Europe/London': 'en-GB',
        'Europe/Berlin': 'de-DE',
        'Europe/Paris': 'fr-FR',
        'Asia/Tokyo': 'ja-JP',
        'Asia/Shanghai': 'zh-CN'
      };

      return tzLocaleMapping[slackUser.tz] || this.config.defaultLocale;
    }

    return this.config.defaultLocale;
  }

  /**
   * Translate Slack message blocks
   */
  translateMessageBlocks(blocks, locale) {
    this.i18n.setLocale(locale);

    const translatedBlocks = blocks.map(block => {
      const newBlock = { ...block };

      if (block.text && block.text.text) {
        newBlock.text = {
          ...block.text,
          text: this.i18n.__(block.text.text)
        };
      }

      if (block.elements) {
        newBlock.elements = block.elements.map(element => {
          const newElement = { ...element };

          if (element.text) {
            newElement.text = this.i18n.__(element.text);
          }

          if (element.placeholder && element.placeholder.text) {
            newElement.placeholder = {
              ...element.placeholder,
              text: this.i18n.__(element.placeholder.text)
            };
          }

          if (element.options) {
            newElement.options = element.options.map(opt => ({
              ...opt,
              text: {
                ...opt.text,
                text: this.i18n.__(opt.text.text)
              }
            }));
          }

          return newElement;
        });
      }

      if (block.fields) {
        newBlock.fields = block.fields.map(field => ({
          ...field,
          text: this.i18n.__(field.text)
        }));
      }

      return newBlock;
    });

    if (this.audit) {
      this.audit.logTranslation({
        system: 'Slack',
        operation: 'translateMessageBlocks',
        locale,
        blockCount: blocks.length
      });
    }

    return translatedBlocks;
  }

  /**
   * Generate localized Slack app manifest
   */
  generateAppManifest(locale, appConfig) {
    this.i18n.setLocale(locale);

    const manifest = {
      display_information: {
        name: this.i18n.__(appConfig.nameKey || 'app.name'),
        description: this.i18n.__(appConfig.descriptionKey || 'app.description'),
        background_color: appConfig.backgroundColor || '#2c2d30',
        long_description: this.i18n.__(appConfig.longDescriptionKey || 'app.long_description')
      },
      features: {},
      settings: {
        org_deploy_enabled: appConfig.orgDeployEnabled || false,
        socket_mode_enabled: appConfig.socketModeEnabled || false
      }
    };

    // Translate bot user display name
    if (appConfig.botUser) {
      manifest.features.bot_user = {
        display_name: this.i18n.__(appConfig.botUser.displayNameKey || 'bot.display_name'),
        always_online: appConfig.botUser.alwaysOnline !== false
      };
    }

    // Translate slash commands
    if (appConfig.slashCommands) {
      manifest.features.slash_commands = appConfig.slashCommands.map(cmd => ({
        command: cmd.command,
        description: this.i18n.__(cmd.descriptionKey || `${cmd.command}.description`),
        usage_hint: this.i18n.__(cmd.usageHintKey || `${cmd.command}.usage_hint`),
        should_escape: cmd.shouldEscape !== false
      }));
    }

    if (this.audit) {
      this.audit.logEvent({
        eventType: 'export',
        system: 'Slack App Manifest',
        locale,
        format: 'JSON'
      });
    }

    return manifest;
  }

  /**
   * Translate Slack home tab view
   */
  translateHomeTabView(viewConfig, locale, user) {
    this.i18n.setLocale(locale);

    const view = {
      type: 'home',
      blocks: this.translateMessageBlocks(viewConfig.blocks, locale)
    };

    if (this.audit) {
      this.audit.logTranslation({
        system: 'Slack Home Tab',
        operation: 'translateHomeTabView',
        locale,
        userId: user.id
      });
    }

    return view;
  }

  /**
   * Translate Slack modal view
   */
  translateModalView(modalConfig, locale) {
    this.i18n.setLocale(locale);

    const modal = {
      type: 'modal',
      callback_id: modalConfig.callbackId,
      title: {
        type: 'plain_text',
        text: this.i18n.__(modalConfig.titleKey || 'modal.title')
      },
      submit: modalConfig.submitKey ? {
        type: 'plain_text',
        text: this.i18n.__(modalConfig.submitKey)
      } : undefined,
      close: {
        type: 'plain_text',
        text: this.i18n.__(modalConfig.closeKey || 'modal.close')
      },
      blocks: this.translateMessageBlocks(modalConfig.blocks, locale)
    };

    return modal;
  }

  /**
   * Create localized Slack message
   */
  createLocalizedMessage(messageKey, locale, params = {}, options = {}) {
    this.i18n.setLocale(locale);

    const message = {
      text: this.i18n.__(messageKey, params),
      ...options
    };

    if (options.blocks) {
      message.blocks = this.translateMessageBlocks(options.blocks, locale);
    }

    if (this.audit) {
      this.audit.logTranslation({
        system: 'Slack Message',
        operation: 'createLocalizedMessage',
        messageKey,
        locale,
        channel: options.channel
      });
    }

    return message;
  }

  /**
   * Translate Slack workflow step
   */
  translateWorkflowStep(stepConfig, locale) {
    this.i18n.setLocale(locale);

    return {
      callback_id: stepConfig.callbackId,
      blocks: this.translateMessageBlocks(stepConfig.blocks, locale),
      submit_label: this.i18n.__(stepConfig.submitLabelKey || 'workflow.submit'),
      outputs: stepConfig.outputs ? stepConfig.outputs.map(output => ({
        type: output.type,
        name: output.name,
        label: this.i18n.__(output.labelKey || `workflow.output.${output.name}`)
      })) : []
    };
  }

  /**
   * Generate localized slash command response
   */
  generateSlashCommandResponse(commandKey, locale, params = {}, responseType = 'ephemeral') {
    this.i18n.setLocale(locale);

    const response = {
      response_type: responseType,
      text: this.i18n.__(commandKey, params)
    };

    if (this.audit) {
      this.audit.logTranslation({
        system: 'Slack Slash Command',
        operation: 'generateSlashCommandResponse',
        commandKey,
        locale,
        responseType
      });
    }

    return response;
  }

  /**
   * Export Slack app translations for distribution
   */
  exportSlackAppTranslations(locale, appKey) {
    const catalog = this.i18n.getCatalog(locale);
    const slackLocale = this.mapI18nLocaleToSlack(locale);

    const appTranslations = {
      app_key: appKey,
      locale: slackLocale,
      translations: {}
    };

    const flatten = (obj, prefix = '') => {
      Object.entries(obj).forEach(([key, value]) => {
        const fullKey = prefix ? `${prefix}.${key}` : key;

        if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
          flatten(value, fullKey);
        } else {
          appTranslations.translations[fullKey] = String(value);
        }
      });
    };

    flatten(catalog);

    return appTranslations;
  }

  /**
   * Middleware for Slack Bolt framework
   */
  boltMiddleware() {
    return async ({ context, body, next }) => {
      // Detect user locale from Slack user info or context
      let locale = this.config.defaultLocale;

      if (body.user && body.user.locale) {
        locale = this.mapSlackLocaleToI18n(body.user.locale);
      } else if (context.userLocale) {
        locale = this.mapSlackLocaleToI18n(context.userLocale);
      }

      this.i18n.setLocale(locale);

      // Add i18n helpers to context
      context.locale = locale;
      context.__ = (...args) => this.i18n.__(...args);
      context.__n = (...args) => this.i18n.__n(...args);
      context.translateBlocks = (blocks) => this.translateMessageBlocks(blocks, locale);
      context.createLocalizedMessage = (key, params, options) =>
        this.createLocalizedMessage(key, locale, params, options);

      await next();
    };
  }

  /**
   * Express middleware for Slack apps
   */
  expressMiddleware() {
    return (req, res, next) => {
      let locale = this.config.defaultLocale;

      // Check Slack payload for locale information
      if (req.body && req.body.user && req.body.user.locale) {
        locale = this.mapSlackLocaleToI18n(req.body.user.locale);
      }

      this.i18n.setLocale(req, locale);

      // Add Slack-specific helpers
      req.slackLocale = this.mapI18nLocaleToSlack(locale);
      req.translateBlocks = (blocks) => this.translateMessageBlocks(blocks, locale);
      req.createLocalizedMessage = (key, params, options) =>
        this.createLocalizedMessage(key, locale, params, options);

      res.locals.slackLocale = req.slackLocale;
      res.locals.i18nLocale = locale;

      next();
    };
  }

  /**
   * Create localized interactive message
   */
  createInteractiveMessage(messageKey, locale, actions = [], params = {}) {
    this.i18n.setLocale(locale);

    const message = {
      text: this.i18n.__(messageKey, params),
      attachments: [{
        text: '',
        fallback: this.i18n.__(messageKey, params),
        callback_id: params.callbackId || 'interactive_message',
        color: params.color || '#3AA3E3',
        attachment_type: 'default',
        actions: actions.map(action => ({
          ...action,
          text: this.i18n.__(action.textKey || action.name),
          confirm: action.confirmKey ? {
            title: this.i18n.__(action.confirmKey + '.title'),
            text: this.i18n.__(action.confirmKey + '.text'),
            ok_text: this.i18n.__(action.confirmKey + '.ok_text'),
            dismiss_text: this.i18n.__(action.confirmKey + '.dismiss_text')
          } : undefined
        }))
      }]
    };

    return message;
  }

  /**
   * Validate Slack payload
   */
  validateSlackPayload(payload) {
    const errors = [];

    if (!payload.type) {
      errors.push('Missing payload type');
    }

    if (payload.type === 'block_actions' && !payload.actions) {
      errors.push('Missing actions array for block_actions');
    }

    if (payload.type === 'view_submission' && !payload.view) {
      errors.push('Missing view for view_submission');
    }

    return {
      valid: errors.length === 0,
      errors
    };
  }
}

module.exports = { SlackI18nAdapter };

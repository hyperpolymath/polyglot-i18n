# Enterprise Integration Adapters

Complete integration adapters for major enterprise systems.

## ERP Systems

### SAP (`erp/sap.js`)
- SAP S/4HANA, ECC, Business One
- Translation Workbench integration
- RFC-compatible bundles
- Fiori middleware
- 15+ language support

### Oracle ERP Cloud (`erp/oracle.js`)
- Oracle Fusion Applications
- OIC (Oracle Integration Cloud) connectors
- OTBI (Oracle Transactional Business Intelligence) integration
- FBDI (File-Based Data Import) format support

### Microsoft Dynamics 365 (`erp/dynamics.js`)
- Dynamics 365 Finance & Operations
- Business Central integration
- Power Platform connectors
- Dataverse localization

## CRM Systems

### Salesforce (`crm/salesforce.js`)
- Custom Labels sync
- Translation Workbench
- Lightning Components
- Visualforce integration

### HubSpot (`crm/hubspot.js`)
- Content translation API
- Email template localization
- Landing page translation
- CMS Hub integration

### ServiceNow (`ais/servicenow.js`)
- Translated Text records
- UI Messages
- Catalog items
- Knowledge base articles

## AIS (Analytics/Intelligence Systems)

### Tableau (`ais/tableau.js`)
- Workbook localization
- Dashboard translation
- Calculated field translations

### Power BI (`ais/powerbi.js`)
- Report translation
- Dataset localization
- Custom visual strings

## SCM (Supply Chain Management)

### Oracle SCM Cloud (`scm/oracle.js`)
- Inventory labels
- Order management
- Procurement translations

### SAP Ariba (`scm/ariba.js`)
- Catalog translations
- Supplier portal localization

## Collaboration Platforms

### Atlassian (`collaboration/atlassian.js`)
- JIRA issue translations
- Confluence page localization
- Bitbucket commit messages

### Slack (`collaboration/slack.js`)
- Bot message translations
- Workspace localization
- Slash command responses

## E-Commerce

### Shopify (`ecommerce/shopify.js`)
- Product translations
- Theme localization
- Checkout translations

### Magento (`ecommerce/magento.js`)
- Store view translations
- Product attributes
- CMS block localization

## All Adapters Support

✅ Bi-directional sync
✅ Audit logging
✅ Batch operations
✅ Webhook integration
✅ API authentication
✅ Rate limiting
✅ Error handling
✅ Retry logic
✅ Data validation
✅ Format conversion

## Usage Pattern

```javascript
const { SAPI18nAdapter } = require('./adapters/erp/sap');

const adapter = new SAPI18nAdapter({
  host: 'sap-host.com',
  client: '100',
  locales: ['EN', 'DE', 'FR']
});

// Translate SAP text
const translated = await adapter.translateSAPText('MAKT-MAKTX', 'DE');

// Middleware for Fiori apps
app.use(adapter.fioriMiddleware());

// Export to SAP format
const sapTable = adapter.exportToSAPTextTable('DE');
```

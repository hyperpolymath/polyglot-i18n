// polyglot-i18n WASM Core - CLDR Plural Rules Engine
// Optimized for size and speed, compliant with Unicode CLDR plural rules
use wasm_bindgen::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Unicode CLDR Plural Categories
/// See: https://unicode.org/reports/tr35/tr35-numbers.html#Language_Plural_Rules
#[wasm_bindgen]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum PluralCategory {
    Zero,
    One,
    Two,
    Few,
    Many,
    Other,
}

impl PluralCategory {
    pub fn as_str(&self) -> &'static str {
        match self {
            PluralCategory::Zero => "zero",
            PluralCategory::One => "one",
            PluralCategory::Two => "two",
            PluralCategory::Few => "few",
            PluralCategory::Many => "many",
            PluralCategory::Other => "other",
        }
    }
}

/// Operands for plural rule evaluation per CLDR spec
/// n = absolute value of the source number
/// i = integer digits of n
/// v = number of visible fraction digits (with trailing zeros)
/// w = number of visible fraction digits (without trailing zeros)
/// f = visible fraction digits (with trailing zeros)
/// t = visible fraction digits (without trailing zeros)
#[derive(Clone, Copy, Debug)]
struct PluralOperands {
    n: f64,  // absolute value
    i: u64,  // integer part
    v: usize, // visible fraction digit count (with trailing zeros)
    w: usize, // visible fraction digit count (without trailing zeros)
    f: u64,  // fraction digits (with trailing zeros)
    t: u64,  // fraction digits (without trailing zeros)
}

impl PluralOperands {
    fn from_f64(num: f64) -> Self {
        let n = num.abs();
        let i = n.floor() as u64;

        // For simplicity, handle up to 6 decimal places
        let frac = n - (i as f64);
        let frac_str = format!("{:.6}", frac);
        let frac_digits: String = frac_str.chars().skip(2).collect();

        let v = frac_digits.len();
        let f: u64 = frac_digits.parse().unwrap_or(0);

        let trimmed = frac_digits.trim_end_matches('0');
        let w = trimmed.len();
        let t: u64 = if trimmed.is_empty() { 0 } else { trimmed.parse().unwrap_or(0) };

        PluralOperands { n, i, v, w, f, t }
    }
}

/// CLDR Plural Rules Engine
/// Implements cardinal plural rules for major languages
#[wasm_bindgen]
pub struct PluralRules {
    locale: String,
}

#[wasm_bindgen]
impl PluralRules {
    #[wasm_bindgen(constructor)]
    pub fn new(locale: &str) -> PluralRules {
        // Normalize locale to base language
        let base = locale.split('-').next().unwrap_or(locale);
        PluralRules {
            locale: base.to_lowercase(),
        }
    }

    /// Select the appropriate plural category for a cardinal number
    #[wasm_bindgen]
    pub fn select(&self, n: f64) -> PluralCategory {
        let op = PluralOperands::from_f64(n);

        match self.locale.as_str() {
            // East Asian (no plural distinctions)
            "ja" | "ko" | "zh" | "vi" | "th" | "lo" | "my" => {
                PluralCategory::Other
            }

            // Germanic languages
            "en" | "de" | "nl" | "sv" | "da" | "no" | "nb" | "nn" => {
                self.rule_one_other(op)
            }

            // Romance languages (French, Italian, Portuguese BR)
            "fr" | "it" | "pt" => {
                self.rule_french(op)
            }

            // Spanish
            "es" => {
                self.rule_spanish(op)
            }

            // Russian and East Slavic
            "ru" | "uk" | "be" => {
                self.rule_russian(op)
            }

            // Polish
            "pl" => {
                self.rule_polish(op)
            }

            // Czech and Slovak
            "cs" | "sk" => {
                self.rule_czech(op)
            }

            // Arabic
            "ar" => {
                self.rule_arabic(op)
            }

            // Hebrew
            "he" | "iw" => {
                self.rule_hebrew(op)
            }

            // Romanian
            "ro" | "mo" => {
                self.rule_romanian(op)
            }

            // Latvian
            "lv" => {
                self.rule_latvian(op)
            }

            // Lithuanian
            "lt" => {
                self.rule_lithuanian(op)
            }

            // Slovenian
            "sl" => {
                self.rule_slovenian(op)
            }

            // Irish
            "ga" => {
                self.rule_irish(op)
            }

            // Welsh
            "cy" => {
                self.rule_welsh(op)
            }

            // Maltese
            "mt" => {
                self.rule_maltese(op)
            }

            // Macedonian
            "mk" => {
                self.rule_macedonian(op)
            }

            // Icelandic
            "is" => {
                self.rule_icelandic(op)
            }

            // Filipino/Tagalog
            "fil" | "tl" => {
                self.rule_filipino(op)
            }

            // Turkish, Azerbaijani (no plural, but has one)
            "tr" | "az" => {
                self.rule_one_other(op)
            }

            // Hindi, Bangla
            "hi" | "bn" => {
                self.rule_hindi(op)
            }

            // Default: one/other
            _ => self.rule_one_other(op)
        }
    }

    /// Get the locale being used
    #[wasm_bindgen(js_name = getLocale)]
    pub fn get_locale(&self) -> String {
        self.locale.clone()
    }

    /// Get category as string
    #[wasm_bindgen(js_name = selectString)]
    pub fn select_string(&self, n: f64) -> String {
        self.select(n).as_str().to_string()
    }
}

// Private rule implementations
impl PluralRules {
    /// one: i=1 and v=0; other
    fn rule_one_other(&self, op: PluralOperands) -> PluralCategory {
        if op.i == 1 && op.v == 0 {
            PluralCategory::One
        } else {
            PluralCategory::Other
        }
    }

    /// French: one for i=0,1; other
    fn rule_french(&self, op: PluralOperands) -> PluralCategory {
        if op.i == 0 || op.i == 1 {
            PluralCategory::One
        } else {
            PluralCategory::Other
        }
    }

    /// Spanish: one: n=1; many: e=0 and end in 6 zeros; other
    fn rule_spanish(&self, op: PluralOperands) -> PluralCategory {
        if op.n == 1.0 {
            PluralCategory::One
        } else if op.i != 0 && op.i % 1000000 == 0 && op.v == 0 {
            PluralCategory::Many
        } else {
            PluralCategory::Other
        }
    }

    /// Russian: complex Slavic rules
    fn rule_russian(&self, op: PluralOperands) -> PluralCategory {
        let mod10 = op.i % 10;
        let mod100 = op.i % 100;

        if op.v == 0 {
            if mod10 == 1 && mod100 != 11 {
                PluralCategory::One
            } else if (2..=4).contains(&mod10) && !(12..=14).contains(&mod100) {
                PluralCategory::Few
            } else if mod10 == 0 || (5..=9).contains(&mod10) || (11..=14).contains(&mod100) {
                PluralCategory::Many
            } else {
                PluralCategory::Other
            }
        } else {
            PluralCategory::Other
        }
    }

    /// Polish: complex rules with few/many
    fn rule_polish(&self, op: PluralOperands) -> PluralCategory {
        let mod10 = op.i % 10;
        let mod100 = op.i % 100;

        if op.i == 1 && op.v == 0 {
            PluralCategory::One
        } else if op.v == 0 && (2..=4).contains(&mod10) && !(12..=14).contains(&mod100) {
            PluralCategory::Few
        } else if op.v == 0 && (op.i != 1 && (0..=1).contains(&mod10))
            || op.v == 0 && (5..=9).contains(&mod10)
            || op.v == 0 && (12..=14).contains(&mod100) {
            PluralCategory::Many
        } else {
            PluralCategory::Other
        }
    }

    /// Czech/Slovak rules
    fn rule_czech(&self, op: PluralOperands) -> PluralCategory {
        if op.i == 1 && op.v == 0 {
            PluralCategory::One
        } else if (2..=4).contains(&op.i) && op.v == 0 {
            PluralCategory::Few
        } else if op.v != 0 {
            PluralCategory::Many
        } else {
            PluralCategory::Other
        }
    }

    /// Arabic: complex rules with zero, one, two, few, many
    fn rule_arabic(&self, op: PluralOperands) -> PluralCategory {
        let mod100 = op.i % 100;

        if op.n == 0.0 {
            PluralCategory::Zero
        } else if op.n == 1.0 {
            PluralCategory::One
        } else if op.n == 2.0 {
            PluralCategory::Two
        } else if (3..=10).contains(&mod100) {
            PluralCategory::Few
        } else if (11..=99).contains(&mod100) {
            PluralCategory::Many
        } else {
            PluralCategory::Other
        }
    }

    /// Hebrew rules
    fn rule_hebrew(&self, op: PluralOperands) -> PluralCategory {
        if op.i == 1 && op.v == 0 {
            PluralCategory::One
        } else if op.i == 2 && op.v == 0 {
            PluralCategory::Two
        } else if op.v == 0 && !(0..=10).contains(&op.i) && op.i % 10 == 0 {
            PluralCategory::Many
        } else {
            PluralCategory::Other
        }
    }

    /// Romanian rules
    fn rule_romanian(&self, op: PluralOperands) -> PluralCategory {
        let mod100 = op.i % 100;

        if op.i == 1 && op.v == 0 {
            PluralCategory::One
        } else if op.v != 0 || op.n == 0.0 || (op.i != 1 && (1..=19).contains(&mod100)) {
            PluralCategory::Few
        } else {
            PluralCategory::Other
        }
    }

    /// Latvian rules
    fn rule_latvian(&self, op: PluralOperands) -> PluralCategory {
        let mod10 = op.i % 10;
        let mod100 = op.i % 100;
        let fmod10 = op.f % 10;
        let fmod100 = op.f % 100;

        if op.n == 0.0 {
            PluralCategory::Zero
        } else if (mod10 == 1 && mod100 != 11) || (op.v == 2 && fmod10 == 1 && fmod100 != 11) || (op.v != 2 && fmod10 == 1) {
            PluralCategory::One
        } else {
            PluralCategory::Other
        }
    }

    /// Lithuanian rules
    fn rule_lithuanian(&self, op: PluralOperands) -> PluralCategory {
        let mod10 = op.i % 10;
        let mod100 = op.i % 100;

        if mod10 == 1 && !(11..=19).contains(&mod100) {
            PluralCategory::One
        } else if (2..=9).contains(&mod10) && !(11..=19).contains(&mod100) {
            PluralCategory::Few
        } else if op.f != 0 {
            PluralCategory::Many
        } else {
            PluralCategory::Other
        }
    }

    /// Slovenian rules
    fn rule_slovenian(&self, op: PluralOperands) -> PluralCategory {
        let mod100 = op.i % 100;

        if op.v == 0 && mod100 == 1 {
            PluralCategory::One
        } else if op.v == 0 && mod100 == 2 {
            PluralCategory::Two
        } else if (op.v == 0 && (3..=4).contains(&mod100)) || op.v != 0 {
            PluralCategory::Few
        } else {
            PluralCategory::Other
        }
    }

    /// Irish rules (complex)
    fn rule_irish(&self, op: PluralOperands) -> PluralCategory {
        if op.n == 1.0 {
            PluralCategory::One
        } else if op.n == 2.0 {
            PluralCategory::Two
        } else if (3.0..=6.0).contains(&op.n) {
            PluralCategory::Few
        } else if (7.0..=10.0).contains(&op.n) {
            PluralCategory::Many
        } else {
            PluralCategory::Other
        }
    }

    /// Welsh rules
    fn rule_welsh(&self, op: PluralOperands) -> PluralCategory {
        if op.n == 0.0 {
            PluralCategory::Zero
        } else if op.n == 1.0 {
            PluralCategory::One
        } else if op.n == 2.0 {
            PluralCategory::Two
        } else if op.n == 3.0 {
            PluralCategory::Few
        } else if op.n == 6.0 {
            PluralCategory::Many
        } else {
            PluralCategory::Other
        }
    }

    /// Maltese rules
    fn rule_maltese(&self, op: PluralOperands) -> PluralCategory {
        let mod100 = op.i % 100;

        if op.n == 1.0 {
            PluralCategory::One
        } else if op.n == 0.0 || (2..=10).contains(&mod100) {
            PluralCategory::Few
        } else if (11..=19).contains(&mod100) {
            PluralCategory::Many
        } else {
            PluralCategory::Other
        }
    }

    /// Macedonian rules
    fn rule_macedonian(&self, op: PluralOperands) -> PluralCategory {
        let mod10 = op.i % 10;
        let mod100 = op.i % 100;
        let fmod10 = op.f % 10;
        let fmod100 = op.f % 100;

        if (op.v == 0 && mod10 == 1 && mod100 != 11) || (fmod10 == 1 && fmod100 != 11) {
            PluralCategory::One
        } else {
            PluralCategory::Other
        }
    }

    /// Icelandic rules
    fn rule_icelandic(&self, op: PluralOperands) -> PluralCategory {
        let mod10 = op.i % 10;
        let mod100 = op.i % 100;

        if op.t == 0 && mod10 == 1 && mod100 != 11 {
            PluralCategory::One
        } else {
            PluralCategory::Other
        }
    }

    /// Filipino/Tagalog rules
    fn rule_filipino(&self, op: PluralOperands) -> PluralCategory {
        let mod10 = op.i % 10;
        let fmod10 = op.f % 10;

        if op.v == 0 && (op.i == 1 || op.i == 2 || op.i == 3)
            || op.v == 0 && mod10 != 4 && mod10 != 6 && mod10 != 9
            || op.v != 0 && fmod10 != 4 && fmod10 != 6 && fmod10 != 9 {
            PluralCategory::One
        } else {
            PluralCategory::Other
        }
    }

    /// Hindi/Bangla rules
    fn rule_hindi(&self, op: PluralOperands) -> PluralCategory {
        if op.i == 0 || op.n == 1.0 {
            PluralCategory::One
        } else {
            PluralCategory::Other
        }
    }
}

/// Main I18n WASM struct with integrated plural rules
#[wasm_bindgen]
pub struct I18nWasm {
    catalogs: HashMap<String, HashMap<String, String>>,
    plural_catalogs: HashMap<String, HashMap<String, PluralForms>>,
    current_locale: String,
    default_locale: String,
    fallbacks: HashMap<String, String>,
}

#[derive(Clone, Serialize, Deserialize)]
struct PluralForms {
    zero: Option<String>,
    one: Option<String>,
    two: Option<String>,
    few: Option<String>,
    many: Option<String>,
    other: String,
}

#[derive(Serialize, Deserialize)]
pub struct Config {
    pub locales: Vec<String>,
    pub default_locale: String,
    #[serde(default)]
    pub fallbacks: HashMap<String, String>,
}

#[wasm_bindgen]
impl I18nWasm {
    #[wasm_bindgen(constructor)]
    pub fn new(config_json: &str) -> Result<I18nWasm, JsValue> {
        let config: Config = serde_json::from_str(config_json)
            .map_err(|e| JsValue::from_str(&format!("Invalid config: {}", e)))?;

        Ok(I18nWasm {
            catalogs: HashMap::new(),
            plural_catalogs: HashMap::new(),
            current_locale: config.default_locale.clone(),
            default_locale: config.default_locale,
            fallbacks: config.fallbacks,
        })
    }

    #[wasm_bindgen(js_name = loadCatalog)]
    pub fn load_catalog(&mut self, locale: &str, catalog_json: &str) -> Result<(), JsValue> {
        let catalog: HashMap<String, serde_json::Value> = serde_json::from_str(catalog_json)
            .map_err(|e| JsValue::from_str(&format!("Invalid catalog: {}", e)))?;

        let mut strings: HashMap<String, String> = HashMap::new();
        let mut plurals: HashMap<String, PluralForms> = HashMap::new();

        // Recursive helper to flatten nested objects with dot notation
        fn flatten_object(
            prefix: &str,
            obj: &serde_json::Map<String, serde_json::Value>,
            strings: &mut HashMap<String, String>,
            plurals: &mut HashMap<String, PluralForms>,
        ) {
            for (key, value) in obj {
                let full_key = if prefix.is_empty() {
                    key.clone()
                } else {
                    format!("{}.{}", prefix, key)
                };

                match value {
                    serde_json::Value::String(s) => {
                        strings.insert(full_key, s.clone());
                    }
                    serde_json::Value::Object(nested) => {
                        // Check if it's plural forms (has "other" key)
                        if nested.contains_key("other") {
                            let forms = PluralForms {
                                zero: nested.get("zero").and_then(|v| v.as_str()).map(String::from),
                                one: nested.get("one").and_then(|v| v.as_str()).map(String::from),
                                two: nested.get("two").and_then(|v| v.as_str()).map(String::from),
                                few: nested.get("few").and_then(|v| v.as_str()).map(String::from),
                                many: nested.get("many").and_then(|v| v.as_str()).map(String::from),
                                other: nested.get("other")
                                    .and_then(|v| v.as_str())
                                    .map(String::from)
                                    .unwrap_or_default(),
                            };
                            plurals.insert(full_key, forms);
                        } else {
                            // Recursively flatten nested objects
                            flatten_object(&full_key, nested, strings, plurals);
                        }
                    }
                    _ => {}
                }
            }
        }

        for (key, value) in catalog {
            match value {
                serde_json::Value::String(s) => {
                    strings.insert(key, s);
                }
                serde_json::Value::Object(obj) => {
                    // Check if it's plural forms (has "other" key)
                    if obj.contains_key("other") {
                        let forms = PluralForms {
                            zero: obj.get("zero").and_then(|v| v.as_str()).map(String::from),
                            one: obj.get("one").and_then(|v| v.as_str()).map(String::from),
                            two: obj.get("two").and_then(|v| v.as_str()).map(String::from),
                            few: obj.get("few").and_then(|v| v.as_str()).map(String::from),
                            many: obj.get("many").and_then(|v| v.as_str()).map(String::from),
                            other: obj.get("other")
                                .and_then(|v| v.as_str())
                                .map(String::from)
                                .unwrap_or_default(),
                        };
                        plurals.insert(key, forms);
                    } else {
                        // Handle nested objects for dot notation
                        flatten_object(&key, &obj, &mut strings, &mut plurals);
                    }
                }
                _ => {}
            }
        }

        self.catalogs.insert(locale.to_string(), strings);
        self.plural_catalogs.insert(locale.to_string(), plurals);
        Ok(())
    }

    #[wasm_bindgen(js_name = translate)]
    pub fn translate(&self, key: &str) -> String {
        // Try current locale
        if let Some(result) = self.try_locale(&self.current_locale, key) {
            return result;
        }

        // Try fallback
        if let Some(fallback) = self.fallbacks.get(&self.current_locale) {
            if let Some(result) = self.try_locale(fallback, key) {
                return result;
            }
        }

        // Try default locale
        if let Some(result) = self.try_locale(&self.default_locale, key) {
            return result;
        }

        // Return key as fallback
        key.to_string()
    }

    fn try_locale(&self, locale: &str, key: &str) -> Option<String> {
        self.catalogs
            .get(locale)
            .and_then(|catalog| catalog.get(key))
            .cloned()
    }

    #[wasm_bindgen(js_name = translatePlural)]
    pub fn translate_plural(&self, key: &str, count: f64) -> String {
        let rules = PluralRules::new(&self.current_locale);
        let category = rules.select(count);

        // Try to find plural forms
        if let Some(forms) = self.plural_catalogs
            .get(&self.current_locale)
            .and_then(|catalog| catalog.get(key))
        {
            let template = match category {
                PluralCategory::Zero => forms.zero.as_ref().unwrap_or(&forms.other),
                PluralCategory::One => forms.one.as_ref().unwrap_or(&forms.other),
                PluralCategory::Two => forms.two.as_ref().unwrap_or(&forms.other),
                PluralCategory::Few => forms.few.as_ref().unwrap_or(&forms.other),
                PluralCategory::Many => forms.many.as_ref().unwrap_or(&forms.other),
                PluralCategory::Other => &forms.other,
            };

            // Replace %d or %s with count
            return template
                .replace("%d", &count.to_string())
                .replace("%s", &count.to_string());
        }

        // Fallback to key
        key.to_string()
    }

    #[wasm_bindgen(js_name = setLocale)]
    pub fn set_locale(&mut self, locale: &str) -> String {
        self.current_locale = locale.to_string();
        locale.to_string()
    }

    #[wasm_bindgen(js_name = getLocale)]
    pub fn get_locale(&self) -> String {
        self.current_locale.clone()
    }

    #[wasm_bindgen(js_name = hasKey)]
    pub fn has_key(&self, key: &str) -> bool {
        self.catalogs
            .get(&self.current_locale)
            .map(|catalog| catalog.contains_key(key))
            .unwrap_or(false)
    }

    #[wasm_bindgen(js_name = getCatalogSize)]
    pub fn get_catalog_size(&self) -> usize {
        self.catalogs
            .get(&self.current_locale)
            .map(|catalog| catalog.len())
            .unwrap_or(0)
    }

    #[wasm_bindgen(js_name = getDefaultLocale)]
    pub fn get_default_locale(&self) -> String {
        self.default_locale.clone()
    }
}

/// Mustache-style interpolation
#[wasm_bindgen(js_name = interpolateMustache)]
pub fn interpolate_mustache(template: &str, values_json: &str) -> Result<String, JsValue> {
    let values: HashMap<String, String> = serde_json::from_str(values_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid values: {}", e)))?;

    let mut result = template.to_string();
    for (key, value) in values {
        result = result.replace(&format!("{{{{{}}}}}", key), &value);
    }
    Ok(result)
}

/// sprintf-style formatting (minimal implementation)
#[wasm_bindgen(js_name = formatSprintf)]
pub fn format_sprintf(template: &str, args_json: &str) -> Result<String, JsValue> {
    let args: Vec<String> = serde_json::from_str(args_json)
        .map_err(|e| JsValue::from_str(&format!("Invalid args: {}", e)))?;

    let mut result = template.to_string();
    for arg in args {
        // Replace first occurrence of %s or %d
        if let Some(pos) = result.find("%s") {
            result = format!("{}{}{}", &result[..pos], arg, &result[pos+2..]);
        } else if let Some(pos) = result.find("%d") {
            result = format!("{}{}{}", &result[..pos], arg, &result[pos+2..]);
        }
    }
    Ok(result)
}

/// Initialize WASM module
#[wasm_bindgen(js_name = initWasm)]
pub fn init() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

/// Get plural category for a number in a locale (standalone function)
#[wasm_bindgen(js_name = getPluralCategory)]
pub fn get_plural_category(locale: &str, n: f64) -> String {
    let rules = PluralRules::new(locale);
    rules.select(n).as_str().to_string()
}

/// Get all supported locales for plural rules
#[wasm_bindgen(js_name = getSupportedPluralLocales)]
pub fn get_supported_plural_locales() -> String {
    let locales = vec![
        "ar", "az", "be", "bn", "cs", "cy", "da", "de", "en", "es",
        "fil", "fr", "ga", "he", "hi", "is", "it", "ja", "ko", "lt",
        "lv", "mk", "mt", "my", "nb", "nl", "nn", "no", "pl", "pt",
        "ro", "ru", "sk", "sl", "sv", "th", "tl", "tr", "uk", "vi", "zh"
    ];
    serde_json::to_string(&locales).unwrap_or_else(|_| "[]".to_string())
}

// ============================================================================
// RelativeTime - Human-readable relative time formatting
// ============================================================================

/// Time units for relative time
#[wasm_bindgen]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TimeUnit {
    Second,
    Minute,
    Hour,
    Day,
    Week,
    Month,
    Year,
}

/// Formatting style
#[wasm_bindgen]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RelativeTimeStyle {
    Long,   // "3 days ago"
    Short,  // "3d ago"
    Narrow, // "3d"
}

/// Numeric display option
#[wasm_bindgen]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NumericOption {
    Always, // "1 day ago"
    Auto,   // "yesterday"
}

/// RelativeTime formatter
#[wasm_bindgen]
pub struct RelativeTimeFormat {
    locale: String,
    style: RelativeTimeStyle,
    numeric: NumericOption,
}

#[wasm_bindgen]
impl RelativeTimeFormat {
    #[wasm_bindgen(constructor)]
    pub fn new(locale: &str) -> RelativeTimeFormat {
        let base = locale.split('-').next().unwrap_or(locale);
        RelativeTimeFormat {
            locale: base.to_lowercase(),
            style: RelativeTimeStyle::Long,
            numeric: NumericOption::Auto,
        }
    }

    #[wasm_bindgen(js_name = setStyle)]
    pub fn set_style(&mut self, style: RelativeTimeStyle) {
        self.style = style;
    }

    #[wasm_bindgen(js_name = setNumeric)]
    pub fn set_numeric(&mut self, numeric: NumericOption) {
        self.numeric = numeric;
    }

    /// Format a difference in seconds
    #[wasm_bindgen]
    pub fn format(&self, diff_seconds: f64) -> String {
        let (unit, value) = self.select_unit(diff_seconds);
        self.format_value(value, unit)
    }

    /// Format with explicit unit
    #[wasm_bindgen(js_name = formatUnit)]
    pub fn format_unit(&self, value: f64, unit: TimeUnit) -> String {
        self.format_value(value, unit)
    }

    fn select_unit(&self, diff_seconds: f64) -> (TimeUnit, f64) {
        let abs_diff = diff_seconds.abs();

        if abs_diff >= 31536000.0 * 0.5 {
            (TimeUnit::Year, diff_seconds / 31536000.0)
        } else if abs_diff >= 2628000.0 * 0.5 {
            (TimeUnit::Month, diff_seconds / 2628000.0)
        } else if abs_diff >= 604800.0 * 0.5 {
            (TimeUnit::Week, diff_seconds / 604800.0)
        } else if abs_diff >= 86400.0 * 0.5 {
            (TimeUnit::Day, diff_seconds / 86400.0)
        } else if abs_diff >= 3600.0 * 0.5 {
            (TimeUnit::Hour, diff_seconds / 3600.0)
        } else if abs_diff >= 60.0 * 0.5 {
            (TimeUnit::Minute, diff_seconds / 60.0)
        } else {
            (TimeUnit::Second, diff_seconds)
        }
    }

    fn format_value(&self, value: f64, unit: TimeUnit) -> String {
        let abs_value = value.abs();
        let rounded = abs_value.round() as i64;
        let is_past = value < 0.0;

        // Check for special cases with Auto numeric
        if self.numeric == NumericOption::Auto {
            if abs_value < 10.0 && unit == TimeUnit::Second {
                return self.get_now_string();
            }
            if rounded == 1 {
                if let Some(special) = self.get_special_name(unit, is_past) {
                    return special;
                }
            }
        }

        let unit_name = self.get_unit_name(unit, rounded);
        self.format_with_direction(rounded, &unit_name, is_past)
    }

    fn get_now_string(&self) -> String {
        match self.locale.as_str() {
            "de" => "gerade eben".to_string(),
            "fr" => "à l'instant".to_string(),
            "es" => "ahora mismo".to_string(),
            "ru" => "только что".to_string(),
            "ja" => "たった今".to_string(),
            "zh" => "刚刚".to_string(),
            "ar" => "الآن".to_string(),
            _ => "just now".to_string(),
        }
    }

    fn get_special_name(&self, unit: TimeUnit, is_past: bool) -> Option<String> {
        match (&self.locale[..], unit, is_past) {
            ("en", TimeUnit::Day, true) => Some("yesterday".to_string()),
            ("en", TimeUnit::Day, false) => Some("tomorrow".to_string()),
            ("en", TimeUnit::Week, true) => Some("last week".to_string()),
            ("en", TimeUnit::Week, false) => Some("next week".to_string()),
            ("en", TimeUnit::Month, true) => Some("last month".to_string()),
            ("en", TimeUnit::Month, false) => Some("next month".to_string()),
            ("en", TimeUnit::Year, true) => Some("last year".to_string()),
            ("en", TimeUnit::Year, false) => Some("next year".to_string()),
            ("de", TimeUnit::Day, true) => Some("gestern".to_string()),
            ("de", TimeUnit::Day, false) => Some("morgen".to_string()),
            ("fr", TimeUnit::Day, true) => Some("hier".to_string()),
            ("fr", TimeUnit::Day, false) => Some("demain".to_string()),
            ("es", TimeUnit::Day, true) => Some("ayer".to_string()),
            ("es", TimeUnit::Day, false) => Some("mañana".to_string()),
            ("ja", TimeUnit::Day, true) => Some("昨日".to_string()),
            ("ja", TimeUnit::Day, false) => Some("明日".to_string()),
            ("zh", TimeUnit::Day, true) => Some("昨天".to_string()),
            ("zh", TimeUnit::Day, false) => Some("明天".to_string()),
            _ => None,
        }
    }

    fn get_unit_name(&self, unit: TimeUnit, count: i64) -> String {
        let rules = PluralRules::new(&self.locale);
        let category = rules.select(count as f64);
        let is_plural = category != PluralCategory::One;

        match (&self.locale[..], unit, self.style, is_plural) {
            // English
            ("en", TimeUnit::Second, RelativeTimeStyle::Long, false) => "second".to_string(),
            ("en", TimeUnit::Second, RelativeTimeStyle::Long, true) => "seconds".to_string(),
            ("en", TimeUnit::Minute, RelativeTimeStyle::Long, false) => "minute".to_string(),
            ("en", TimeUnit::Minute, RelativeTimeStyle::Long, true) => "minutes".to_string(),
            ("en", TimeUnit::Hour, RelativeTimeStyle::Long, false) => "hour".to_string(),
            ("en", TimeUnit::Hour, RelativeTimeStyle::Long, true) => "hours".to_string(),
            ("en", TimeUnit::Day, RelativeTimeStyle::Long, false) => "day".to_string(),
            ("en", TimeUnit::Day, RelativeTimeStyle::Long, true) => "days".to_string(),
            ("en", TimeUnit::Week, RelativeTimeStyle::Long, false) => "week".to_string(),
            ("en", TimeUnit::Week, RelativeTimeStyle::Long, true) => "weeks".to_string(),
            ("en", TimeUnit::Month, RelativeTimeStyle::Long, false) => "month".to_string(),
            ("en", TimeUnit::Month, RelativeTimeStyle::Long, true) => "months".to_string(),
            ("en", TimeUnit::Year, RelativeTimeStyle::Long, false) => "year".to_string(),
            ("en", TimeUnit::Year, RelativeTimeStyle::Long, true) => "years".to_string(),
            // Short/Narrow English
            ("en", TimeUnit::Second, RelativeTimeStyle::Short, _) => "sec".to_string(),
            ("en", TimeUnit::Minute, RelativeTimeStyle::Short, _) => "min".to_string(),
            ("en", TimeUnit::Hour, RelativeTimeStyle::Short, _) => "hr".to_string(),
            ("en", TimeUnit::Day, RelativeTimeStyle::Short, _) => "day".to_string(),
            ("en", TimeUnit::Week, RelativeTimeStyle::Short, _) => "wk".to_string(),
            ("en", TimeUnit::Month, RelativeTimeStyle::Short, _) => "mo".to_string(),
            ("en", TimeUnit::Year, RelativeTimeStyle::Short, _) => "yr".to_string(),
            ("en", TimeUnit::Second, RelativeTimeStyle::Narrow, _) => "s".to_string(),
            ("en", TimeUnit::Minute, RelativeTimeStyle::Narrow, _) => "m".to_string(),
            ("en", TimeUnit::Hour, RelativeTimeStyle::Narrow, _) => "h".to_string(),
            ("en", TimeUnit::Day, RelativeTimeStyle::Narrow, _) => "d".to_string(),
            ("en", TimeUnit::Week, RelativeTimeStyle::Narrow, _) => "w".to_string(),
            ("en", TimeUnit::Month, RelativeTimeStyle::Narrow, _) => "mo".to_string(),
            ("en", TimeUnit::Year, RelativeTimeStyle::Narrow, _) => "y".to_string(),
            // Default fallback
            (_, TimeUnit::Second, _, _) => "seconds".to_string(),
            (_, TimeUnit::Minute, _, _) => "minutes".to_string(),
            (_, TimeUnit::Hour, _, _) => "hours".to_string(),
            (_, TimeUnit::Day, _, _) => "days".to_string(),
            (_, TimeUnit::Week, _, _) => "weeks".to_string(),
            (_, TimeUnit::Month, _, _) => "months".to_string(),
            (_, TimeUnit::Year, _, _) => "years".to_string(),
        }
    }

    fn format_with_direction(&self, value: i64, unit_name: &str, is_past: bool) -> String {
        match self.locale.as_str() {
            "ja" | "zh" | "ko" => {
                let marker = if is_past { "前" } else { "後" };
                format!("{}{}{}", value, unit_name, marker)
            }
            "de" => {
                if is_past {
                    format!("vor {} {}", value, unit_name)
                } else {
                    format!("in {} {}", value, unit_name)
                }
            }
            "fr" => {
                if is_past {
                    format!("il y a {} {}", value, unit_name)
                } else {
                    format!("dans {} {}", value, unit_name)
                }
            }
            "es" => {
                if is_past {
                    format!("hace {} {}", value, unit_name)
                } else {
                    format!("en {} {}", value, unit_name)
                }
            }
            "ru" => {
                if is_past {
                    format!("{} {} назад", value, unit_name)
                } else {
                    format!("через {} {}", value, unit_name)
                }
            }
            _ => {
                // English default
                if is_past {
                    format!("{} {} ago", value, unit_name)
                } else {
                    format!("in {} {}", value, unit_name)
                }
            }
        }
    }
}

/// Format relative time from timestamp (standalone function)
#[wasm_bindgen(js_name = formatRelativeTime)]
pub fn format_relative_time(locale: &str, diff_seconds: f64) -> String {
    let formatter = RelativeTimeFormat::new(locale);
    formatter.format(diff_seconds)
}

// ============================================================================
// FuzzyMatch - Approximate string matching (agrep-style)
// ============================================================================

/// Fuzzy matching result
#[derive(Clone, Serialize, Deserialize)]
pub struct FuzzyMatch {
    pub text: String,
    pub score: f64,      // 0.0 to 1.0 similarity
    pub distance: usize, // Levenshtein edit distance
}

/// FuzzyMatcher - Translation memory style matching
#[wasm_bindgen]
pub struct FuzzyMatcher {
    threshold: f64,
    max_results: usize,
}

#[wasm_bindgen]
impl FuzzyMatcher {
    #[wasm_bindgen(constructor)]
    pub fn new() -> FuzzyMatcher {
        FuzzyMatcher {
            threshold: 0.6,  // 60% similarity minimum
            max_results: 10,
        }
    }

    #[wasm_bindgen(js_name = setThreshold)]
    pub fn set_threshold(&mut self, threshold: f64) {
        self.threshold = threshold.clamp(0.0, 1.0);
    }

    #[wasm_bindgen(js_name = setMaxResults)]
    pub fn set_max_results(&mut self, max: usize) {
        self.max_results = max;
    }

    /// Calculate Levenshtein distance between two strings
    #[wasm_bindgen(js_name = levenshteinDistance)]
    pub fn levenshtein_distance(a: &str, b: &str) -> usize {
        levenshtein(a, b)
    }

    /// Calculate similarity score (0.0 to 1.0)
    #[wasm_bindgen]
    pub fn similarity(a: &str, b: &str) -> f64 {
        let dist = levenshtein(a, b);
        let max_len = a.len().max(b.len());
        if max_len == 0 {
            return 1.0;
        }
        1.0 - (dist as f64 / max_len as f64)
    }

    /// Find matches in a corpus (JSON array of strings)
    #[wasm_bindgen(js_name = findMatches)]
    pub fn find_matches(&self, query: &str, corpus_json: &str) -> Result<String, JsValue> {
        let corpus: Vec<String> = serde_json::from_str(corpus_json)
            .map_err(|e| JsValue::from_str(&format!("Invalid corpus: {}", e)))?;

        let query_lower = query.to_lowercase();
        let mut matches: Vec<FuzzyMatch> = corpus
            .iter()
            .map(|text| {
                let text_lower = text.to_lowercase();
                let dist = levenshtein(&query_lower, &text_lower);
                let max_len = query_lower.len().max(text_lower.len());
                let score = if max_len == 0 {
                    1.0
                } else {
                    1.0 - (dist as f64 / max_len as f64)
                };
                FuzzyMatch {
                    text: text.clone(),
                    score,
                    distance: dist,
                }
            })
            .filter(|m| m.score >= self.threshold)
            .collect();

        matches.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap_or(std::cmp::Ordering::Equal));
        matches.truncate(self.max_results);

        serde_json::to_string(&matches)
            .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
    }

    /// agrep-style matching with max edit distance
    #[wasm_bindgen]
    pub fn agrep(pattern: &str, text: &str, max_distance: usize) -> bool {
        levenshtein(pattern, text) <= max_distance
    }

    /// Find best match from candidates
    #[wasm_bindgen(js_name = bestMatch)]
    pub fn best_match(&self, query: &str, candidates_json: &str) -> Result<String, JsValue> {
        let candidates: Vec<String> = serde_json::from_str(candidates_json)
            .map_err(|e| JsValue::from_str(&format!("Invalid candidates: {}", e)))?;

        let query_lower = query.to_lowercase();
        let best = candidates
            .iter()
            .map(|c| {
                let c_lower = c.to_lowercase();
                let dist = levenshtein(&query_lower, &c_lower);
                let max_len = query_lower.len().max(c_lower.len());
                let score = if max_len == 0 {
                    1.0
                } else {
                    1.0 - (dist as f64 / max_len as f64)
                };
                (c.clone(), score, dist)
            })
            .max_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));

        match best {
            Some((text, score, distance)) if score >= self.threshold => {
                let result = FuzzyMatch { text, score, distance };
                serde_json::to_string(&result)
                    .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
            }
            _ => Ok("null".to_string()),
        }
    }
}

/// Levenshtein distance implementation (optimized)
fn levenshtein(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let a_len = a_chars.len();
    let b_len = b_chars.len();

    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }

    // Use two rows for space optimization
    let mut prev_row: Vec<usize> = (0..=b_len).collect();
    let mut curr_row: Vec<usize> = vec![0; b_len + 1];

    for (i, a_char) in a_chars.iter().enumerate() {
        curr_row[0] = i + 1;

        for (j, b_char) in b_chars.iter().enumerate() {
            let cost = if a_char == b_char { 0 } else { 1 };
            curr_row[j + 1] = (prev_row[j + 1] + 1)      // deletion
                .min(curr_row[j] + 1)                    // insertion
                .min(prev_row[j] + cost);                // substitution
        }

        std::mem::swap(&mut prev_row, &mut curr_row);
    }

    prev_row[b_len]
}

/// Damerau-Levenshtein (includes transpositions)
#[wasm_bindgen(js_name = damerauLevenshtein)]
pub fn damerau_levenshtein(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let a_len = a_chars.len();
    let b_len = b_chars.len();

    if a_len == 0 {
        return b_len;
    }
    if b_len == 0 {
        return a_len;
    }

    let mut matrix: Vec<Vec<usize>> = vec![vec![0; b_len + 1]; a_len + 1];

    for i in 0..=a_len {
        matrix[i][0] = i;
    }
    for j in 0..=b_len {
        matrix[0][j] = j;
    }

    for i in 1..=a_len {
        for j in 1..=b_len {
            let cost = if a_chars[i - 1] == b_chars[j - 1] { 0 } else { 1 };

            matrix[i][j] = (matrix[i - 1][j] + 1)      // deletion
                .min(matrix[i][j - 1] + 1)            // insertion
                .min(matrix[i - 1][j - 1] + cost);    // substitution

            // Transposition
            if i > 1 && j > 1
                && a_chars[i - 1] == b_chars[j - 2]
                && a_chars[i - 2] == b_chars[j - 1]
            {
                matrix[i][j] = matrix[i][j].min(matrix[i - 2][j - 2] + cost);
            }
        }
    }

    matrix[a_len][b_len]
}

// ============================================================================
// Stemming - Word root extraction for better fuzzy matches
// ============================================================================

/// Porter Stemmer (English) - simplified implementation
#[wasm_bindgen]
pub struct Stemmer {
    locale: String,
}

#[wasm_bindgen]
impl Stemmer {
    #[wasm_bindgen(constructor)]
    pub fn new(locale: &str) -> Stemmer {
        Stemmer {
            locale: locale.split('-').next().unwrap_or(locale).to_lowercase(),
        }
    }

    /// Stem a single word
    #[wasm_bindgen]
    pub fn stem(&self, word: &str) -> String {
        match self.locale.as_str() {
            "en" => stem_english(word),
            "de" => stem_german(word),
            "fr" => stem_french(word),
            "es" => stem_spanish(word),
            _ => word.to_lowercase(),
        }
    }

    /// Stem multiple words (JSON array)
    #[wasm_bindgen(js_name = stemWords)]
    pub fn stem_words(&self, words_json: &str) -> Result<String, JsValue> {
        let words: Vec<String> = serde_json::from_str(words_json)
            .map_err(|e| JsValue::from_str(&format!("Invalid words: {}", e)))?;

        let stemmed: Vec<String> = words.iter().map(|w| self.stem(w)).collect();

        serde_json::to_string(&stemmed)
            .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
    }
}

/// English Porter Stemmer (simplified)
fn stem_english(word: &str) -> String {
    let mut s = word.to_lowercase();

    // Step 1a: sses -> ss, ies -> i, ss -> ss, s -> (remove)
    if s.ends_with("sses") {
        s.truncate(s.len() - 2);
    } else if s.ends_with("ies") {
        s.truncate(s.len() - 2);
    } else if s.ends_with('s') && !s.ends_with("ss") && s.len() > 3 {
        s.pop();
    }

    // Step 1b: eed -> ee, ed -> (remove if contains vowel), ing -> (remove if contains vowel)
    if s.ends_with("eed") {
        if s.len() > 4 {
            s.truncate(s.len() - 1);
        }
    } else if s.ends_with("ed") && has_vowel(&s[..s.len()-2]) {
        s.truncate(s.len() - 2);
    } else if s.ends_with("ing") && has_vowel(&s[..s.len()-3]) {
        s.truncate(s.len() - 3);
    }

    // Step 2: Remove common suffixes
    let suffixes = [
        ("ational", "ate"), ("tional", "tion"), ("enci", "ence"),
        ("anci", "ance"), ("izer", "ize"), ("abli", "able"),
        ("alli", "al"), ("entli", "ent"), ("eli", "e"),
        ("ousli", "ous"), ("ization", "ize"), ("ation", "ate"),
        ("ator", "ate"), ("alism", "al"), ("iveness", "ive"),
        ("fulness", "ful"), ("ousness", "ous"), ("aliti", "al"),
        ("iviti", "ive"), ("biliti", "ble"),
    ];

    for (suffix, replacement) in suffixes.iter() {
        if s.ends_with(suffix) && s.len() > suffix.len() + 2 {
            s.truncate(s.len() - suffix.len());
            s.push_str(replacement);
            break;
        }
    }

    // Step 3: Remove more suffixes
    let step3 = [
        ("icate", "ic"), ("ative", ""), ("alize", "al"),
        ("iciti", "ic"), ("ical", "ic"), ("ful", ""), ("ness", ""),
    ];

    for (suffix, replacement) in step3.iter() {
        if s.ends_with(suffix) && s.len() > suffix.len() + 2 {
            s.truncate(s.len() - suffix.len());
            s.push_str(replacement);
            break;
        }
    }

    s
}

fn has_vowel(s: &str) -> bool {
    s.chars().any(|c| matches!(c, 'a' | 'e' | 'i' | 'o' | 'u'))
}

/// German stemmer (simplified Snowball-style)
fn stem_german(word: &str) -> String {
    let mut s = word.to_lowercase();

    // Remove common German suffixes
    let suffixes = ["erin", "ern", "em", "en", "er", "es", "e", "s"];
    for suffix in suffixes.iter() {
        if s.ends_with(suffix) && s.len() > suffix.len() + 2 {
            s.truncate(s.len() - suffix.len());
            break;
        }
    }

    s
}

/// French stemmer (simplified)
fn stem_french(word: &str) -> String {
    let mut s = word.to_lowercase();

    let suffixes = ["issements", "issement", "atrices", "atrice", "ateur",
                    "ations", "ation", "ences", "ence", "ments", "ment",
                    "ités", "ité", "ives", "ive", "eaux", "aux", "euses",
                    "euse", "eux", "es", "é", "e"];

    for suffix in suffixes.iter() {
        if s.ends_with(suffix) && s.len() > suffix.len() + 2 {
            s.truncate(s.len() - suffix.len());
            break;
        }
    }

    s
}

/// Spanish stemmer (simplified)
fn stem_spanish(word: &str) -> String {
    let mut s = word.to_lowercase();

    let suffixes = ["amientos", "imientos", "amiento", "imiento",
                    "aciones", "ación", "adoras", "adores", "adora",
                    "ador", "ancias", "ancia", "antes", "ante",
                    "ables", "able", "ibles", "ible", "istas", "ista",
                    "mente", "idad", "ivos", "iva", "ivo", "osas",
                    "oso", "osa", "es", "os", "as", "a", "o"];

    for suffix in suffixes.iter() {
        if s.ends_with(suffix) && s.len() > suffix.len() + 2 {
            s.truncate(s.len() - suffix.len());
            break;
        }
    }

    s
}

// ============================================================================
// Segmentation - Sentence and word breaking
// ============================================================================

/// Text segmenter for sentence/word boundaries
#[wasm_bindgen]
pub struct Segmenter {
    locale: String,
}

#[wasm_bindgen]
impl Segmenter {
    #[wasm_bindgen(constructor)]
    pub fn new(locale: &str) -> Segmenter {
        Segmenter {
            locale: locale.split('-').next().unwrap_or(locale).to_lowercase(),
        }
    }

    /// Split text into sentences
    #[wasm_bindgen(js_name = segmentSentences)]
    pub fn segment_sentences(&self, text: &str) -> Result<String, JsValue> {
        let sentences = self.split_sentences(text);
        serde_json::to_string(&sentences)
            .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
    }

    /// Split text into words
    #[wasm_bindgen(js_name = segmentWords)]
    pub fn segment_words(&self, text: &str) -> Result<String, JsValue> {
        let words = self.split_words(text);
        serde_json::to_string(&words)
            .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
    }

    /// Get word count
    #[wasm_bindgen(js_name = wordCount)]
    pub fn word_count(&self, text: &str) -> usize {
        self.split_words(text).len()
    }

    /// Get sentence count
    #[wasm_bindgen(js_name = sentenceCount)]
    pub fn sentence_count(&self, text: &str) -> usize {
        self.split_sentences(text).len()
    }

    fn split_sentences(&self, text: &str) -> Vec<String> {
        // Handle CJK languages (no spaces between words)
        if matches!(self.locale.as_str(), "ja" | "zh" | "ko") {
            return self.split_sentences_cjk(text);
        }

        // Western languages - split on sentence-ending punctuation
        let mut sentences = Vec::new();
        let mut current = String::new();
        let chars: Vec<char> = text.chars().collect();

        for (i, &c) in chars.iter().enumerate() {
            current.push(c);

            if matches!(c, '.' | '!' | '?' | '。' | '！' | '？') {
                // Check it's not an abbreviation (followed by lowercase)
                let next_char = chars.get(i + 1).copied();
                let next_next = chars.get(i + 2).copied();

                let is_end = match (next_char, next_next) {
                    (None, _) => true,
                    (Some(' '), Some(nc)) if nc.is_uppercase() => true,
                    (Some(' '), None) => true,
                    (Some('\n'), _) => true,
                    _ => false,
                };

                if is_end {
                    let trimmed = current.trim().to_string();
                    if !trimmed.is_empty() {
                        sentences.push(trimmed);
                    }
                    current.clear();
                }
            }
        }

        let trimmed = current.trim().to_string();
        if !trimmed.is_empty() {
            sentences.push(trimmed);
        }

        sentences
    }

    fn split_sentences_cjk(&self, text: &str) -> Vec<String> {
        let mut sentences = Vec::new();
        let mut current = String::new();

        for c in text.chars() {
            current.push(c);
            if matches!(c, '。' | '！' | '？' | '.' | '!' | '?') {
                let trimmed = current.trim().to_string();
                if !trimmed.is_empty() {
                    sentences.push(trimmed);
                }
                current.clear();
            }
        }

        let trimmed = current.trim().to_string();
        if !trimmed.is_empty() {
            sentences.push(trimmed);
        }

        sentences
    }

    fn split_words(&self, text: &str) -> Vec<String> {
        // CJK: each character is approximately a "word"
        if matches!(self.locale.as_str(), "ja" | "zh" | "ko") {
            return text
                .chars()
                .filter(|c| !c.is_whitespace() && !c.is_ascii_punctuation())
                .map(|c| c.to_string())
                .collect();
        }

        // Western languages: split on whitespace and punctuation
        text.split(|c: char| c.is_whitespace() || matches!(c, ',' | ';' | ':' | '"' | '\'' | '(' | ')' | '[' | ']'))
            .filter(|s| !s.is_empty())
            .map(|s| s.trim_matches(|c: char| c.is_ascii_punctuation()).to_string())
            .filter(|s| !s.is_empty())
            .collect()
    }
}

// ============================================================================
// N-gram generation for fuzzy matching
// ============================================================================

/// Generate n-grams from text
#[wasm_bindgen(js_name = generateNgrams)]
pub fn generate_ngrams(text: &str, n: usize) -> Result<String, JsValue> {
    let chars: Vec<char> = text.to_lowercase().chars().collect();
    if chars.len() < n {
        return serde_json::to_string(&vec![text.to_lowercase()])
            .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)));
    }

    let ngrams: Vec<String> = chars
        .windows(n)
        .map(|w| w.iter().collect())
        .collect();

    serde_json::to_string(&ngrams)
        .map_err(|e| JsValue::from_str(&format!("Serialization error: {}", e)))
}

/// Jaccard similarity using n-grams
#[wasm_bindgen(js_name = ngramSimilarity)]
pub fn ngram_similarity(a: &str, b: &str, n: usize) -> f64 {
    let a_chars: Vec<char> = a.to_lowercase().chars().collect();
    let b_chars: Vec<char> = b.to_lowercase().chars().collect();

    if a_chars.len() < n || b_chars.len() < n {
        return if a.to_lowercase() == b.to_lowercase() { 1.0 } else { 0.0 };
    }

    let a_ngrams: std::collections::HashSet<String> = a_chars
        .windows(n)
        .map(|w| w.iter().collect())
        .collect();

    let b_ngrams: std::collections::HashSet<String> = b_chars
        .windows(n)
        .map(|w| w.iter().collect())
        .collect();

    let intersection = a_ngrams.intersection(&b_ngrams).count();
    let union = a_ngrams.union(&b_ngrams).count();

    if union == 0 {
        return 1.0;
    }

    intersection as f64 / union as f64
}

// ============================================================================
// External tool configuration (for Deno FFI)
// ============================================================================

/// External tool paths configuration
#[derive(Clone, Serialize, Deserialize)]
pub struct ExternalToolConfig {
    pub pandoc_path: Option<String>,
    pub tesseract_path: Option<String>,
    pub agrep_path: Option<String>,
}

/// Get recommended command for Pandoc extraction
#[wasm_bindgen(js_name = getPandocCommand)]
pub fn get_pandoc_command(input_file: &str, output_format: &str) -> String {
    format!(
        "pandoc -t {} --wrap=none {}",
        output_format,
        shell_escape(input_file)
    )
}

/// Get recommended command for Tesseract OCR
#[wasm_bindgen(js_name = getTesseractCommand)]
pub fn get_tesseract_command(input_file: &str, lang: &str) -> String {
    format!(
        "tesseract {} stdout -l {}",
        shell_escape(input_file),
        lang
    )
}

fn shell_escape(s: &str) -> String {
    format!("'{}'", s.replace('\'', "'\\''"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_english_plurals() {
        let rules = PluralRules::new("en");
        assert_eq!(rules.select(1.0), PluralCategory::One);
        assert_eq!(rules.select(0.0), PluralCategory::Other);
        assert_eq!(rules.select(2.0), PluralCategory::Other);
        assert_eq!(rules.select(1.5), PluralCategory::Other);
    }

    #[test]
    fn test_russian_plurals() {
        let rules = PluralRules::new("ru");
        assert_eq!(rules.select(1.0), PluralCategory::One);
        assert_eq!(rules.select(2.0), PluralCategory::Few);
        assert_eq!(rules.select(5.0), PluralCategory::Many);
        assert_eq!(rules.select(21.0), PluralCategory::One);
        assert_eq!(rules.select(22.0), PluralCategory::Few);
        assert_eq!(rules.select(25.0), PluralCategory::Many);
    }

    #[test]
    fn test_arabic_plurals() {
        let rules = PluralRules::new("ar");
        assert_eq!(rules.select(0.0), PluralCategory::Zero);
        assert_eq!(rules.select(1.0), PluralCategory::One);
        assert_eq!(rules.select(2.0), PluralCategory::Two);
        assert_eq!(rules.select(3.0), PluralCategory::Few);
        assert_eq!(rules.select(11.0), PluralCategory::Many);
    }

    #[test]
    fn test_japanese_plurals() {
        let rules = PluralRules::new("ja");
        assert_eq!(rules.select(0.0), PluralCategory::Other);
        assert_eq!(rules.select(1.0), PluralCategory::Other);
        assert_eq!(rules.select(100.0), PluralCategory::Other);
    }

    #[test]
    fn test_levenshtein() {
        assert_eq!(levenshtein("kitten", "sitting"), 3);
        assert_eq!(levenshtein("", "abc"), 3);
        assert_eq!(levenshtein("abc", "abc"), 0);
        assert_eq!(levenshtein("hello", "hallo"), 1);
    }

    #[test]
    fn test_stemmer() {
        let stemmer = Stemmer::new("en");
        assert_eq!(stemmer.stem("running"), "run");
        assert_eq!(stemmer.stem("cats"), "cat");
        assert_eq!(stemmer.stem("happiness"), "happi");
    }

    #[test]
    fn test_segmenter() {
        let seg = Segmenter::new("en");
        assert_eq!(seg.word_count("Hello world!"), 2);
        assert_eq!(seg.sentence_count("Hello. World!"), 2);
    }
}

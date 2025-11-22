# Framework Integration Examples

Comprehensive examples for 25+ modern frameworks and runtimes.

## Frontend Frameworks

### React Ecosystem
- ✅ **Next.js** - SSR/SSG with i18n routing (`nextjs/`)
- ✅ **Remix** - Full-stack React framework (`remix/`)

### Vue Ecosystem
- ✅ **Nuxt.js** - Vue SSR framework (`nuxt/`)
- ✅ **Vue 3** - Composition API integration (`vue/`)

### Others
- ✅ **Angular** - Dependency injection (`angular/`)
- ✅ **Svelte/SvelteKit** - Reactive i18n (`svelte/`)
- ✅ **SolidJS** - Fine-grained reactivity (`solidjs/`)
- ✅ **Qwik** - Resumable framework (`qwik/`)
- ✅ **Astro** - Islands architecture (`astro/`)

## Backend Frameworks

### Node.js
- ✅ **Express** - Classic middleware (`express4-cookie/`, `express4-setLocale/`)
- ✅ **Fastify** - High performance (`fastify/`)
- ✅ **Koa** - Next-gen middleware (`koa/`)
- ✅ **NestJS** - Enterprise TypeScript (`nestjs/`)
- ✅ **AdonisJS** - Full-stack MVC (`adonis/`)
- ✅ **FeathersJS** - Real-time services (`feathers/`)
- ✅ **LoopBack** - API framework (`loopback/`)
- ✅ **Hono** - Ultrafast web framework (`hono/`)

### Deno
- ✅ **Oak** - Deno middleware (`deno/examples/oak.ts`)
- ✅ **Fresh** - Deno web framework (`fresh/`)

### Bun
- ✅ **Bun HTTP** - Native Bun server (`bun/`)
- ✅ **Elysia** - Bun web framework (`bun/elysia.ts`)

## Meta-Frameworks

- ✅ **Next.js** - React SSR/SSG
- ✅ **Nuxt** - Vue SSR/SSG
- ✅ **SvelteKit** - Svelte SSR
- ✅ **Remix** - React full-stack
- ✅ **Astro** - Multi-framework
- ✅ **Qwik City** - Qwik meta-framework

## Runtimes

- ✅ **Node.js** - 10.x - 20.x
- ✅ **Deno** - Native ESM
- ✅ **Bun** - Ultra-fast runtime
- ✅ **WASM** - WebAssembly core

## Integration Patterns

### Client-Side
```javascript
// Vue 3 Composition API
import { useI18n } from './composables/useI18n';

export default {
  setup() {
    const { t, locale, setLocale } = useI18n();

    return { t, locale, setLocale };
  }
}
```

### Server-Side
```javascript
// Fastify
import { createI18n } from 'i18n';

const i18n = createI18n({ locales: ['en', 'de'] });
app.decorateRequest('i18n', i18n);
```

### Universal (Isomorphic)
```javascript
// Next.js
export async function getServerSideProps({ locale }) {
  const i18n = createI18n({ locale });
  return { props: { translations: i18n.getCatalog() } };
}
```

### Edge Runtime
```javascript
// Cloudflare Workers / Vercel Edge
export default {
  async fetch(request, env) {
    const i18n = createI18n({ staticCatalog: env.TRANSLATIONS });
    return new Response(i18n.__('hello'));
  }
}
```

## Features by Framework

| Framework | SSR | SSG | Edge | WASM | Streaming |
|-----------|-----|-----|------|------|-----------|
| Next.js   | ✅  | ✅  | ✅   | ✅   | ✅        |
| Nuxt      | ✅  | ✅  | ✅   | ✅   | ✅        |
| SvelteKit | ✅  | ✅  | ✅   | ✅   | ✅        |
| Remix     | ✅  | ❌  | ✅   | ✅   | ✅        |
| Astro     | ✅  | ✅  | ✅   | ✅   | ❌        |
| Qwik      | ✅  | ✅  | ✅   | ✅   | ✅        |
| Fresh     | ✅  | ❌  | ✅   | ✅   | ✅        |

## Testing

Each example includes:
- Unit tests
- Integration tests
- E2E tests (where applicable)
- Performance benchmarks

## Quick Start

```bash
# Pick a framework
cd examples/nextjs

# Install dependencies
npm install

# Run example
npm run dev

# Visit http://localhost:3000
```

## Contribution

To add a new framework:
1. Create directory in `examples/`
2. Add README with setup instructions
3. Include working example
4. Add tests
5. Update this file

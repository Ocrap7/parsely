# HyperMedia Templating Language (HMTL)

HMTL is a modern way to write hypermedia applications that aims to replace the legacy HTML

### Usage
Compile a file into output directory dist:
```bash
hmtlc build src/view.par -o dist
```

Compile multiple files:
```bash
hmtlc build src/view.par src/another_view.par # output directory is `dist` by default
```

Compile directory:
```bash
hmtlc build src # compiles all files and sub directories in `src`
```

### Typescript Backend

Compile this:
```ts
input {
    value: ${number}
}

html {
    head {
        title 'This title';
    }

    body {
        a (href: '/route') {
            ${
                5
            }
            'Goto route'
        }
    }
}
```

into this:

```ts
import Context from './context.ts'

export type Input = {
    value: number
}

export default function render<C extends Context>(ctx: C, { value, }: Input) {
    return 
`<html>
    <head>
    <title>This title</title>
    </head>

    <body>
        <a href="/route">
        ${5} Goto route
        </a>
    </body>
</html>`
}
```
export type Input = {
value: number
}
import Context from '../../context.ts'
export default function render<C extends Context>(ctx: C, { value, }: Input) {return `<html><head><title>This title</title></head><body><a href="/route"fdfsd="fdjkslf">${34434}Goto route</a></body></html>`}
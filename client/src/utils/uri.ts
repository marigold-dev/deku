/**
 * Convert an http uri to a ws uri
 * @param uri an url starting by http or https
 * @returns the same uri but starting with ws or wss
 */
const httpToWs = (uri: string): string => uri
    .replace("https", "wss")
    .replace("http", "ws")

export default {
    httpToWs
}
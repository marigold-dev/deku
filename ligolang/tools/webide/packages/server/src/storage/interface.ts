export interface FileStorage {
  read(filename: string): Promise<string>;
  write(filename: string, content: string): Promise<void>;
}

export class FileNotFoundError extends Error {}

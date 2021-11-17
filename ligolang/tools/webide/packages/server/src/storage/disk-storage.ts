import fs from 'fs';
import { join } from 'path';
import { FileStorage, FileNotFoundError } from './interface';

export class DiskStorage implements FileStorage {
  constructor(private readonly directory: string) {}

  async write(filename: string, content: string) {
    const path = join(this.directory, filename);
    return fs.writeFileSync(path, content);
  }

  async read(filename: string): Promise<string> {
    const path = join(this.directory, filename);

    if (!fs.existsSync(path)) {
      throw new FileNotFoundError(`File not found ${path}`);
    }

    const buf = fs.readFileSync(path);
    return buf.toString();
  }
}

import { FileStorage, FileNotFoundError } from './interface';
const { Storage } = require('@google-cloud/storage');
export class GoogleStorage implements FileStorage {
  private storage = new Storage();

  constructor(private readonly bucket: string) {}

  async write(filename: string, content: string) {
    const stream = this.storage
      .bucket(this.bucket)
      .file(filename)
      .createWriteStream({});

    stream.end(Buffer.from(content));
  }

  async read(filename: string): Promise<string> {
    return new Promise((resolve, reject) => {
      try {
        const file = this.storage.bucket(this.bucket).file(filename);
        file.exists(function(err: Error, exists: any) {
          if (err) {
            reject(err);
          }

          if (!exists) {
            reject(new FileNotFoundError(`File not found ${filename}`));
          } else {
            const stream = file.createReadStream();
            var buf = '';
            stream
              .on('data', function(d: string) {
                buf += d;
              })
              .on('end', function() {
                resolve(buf.toString());
              });
          }
        });
      } catch (ex) {
        reject(ex);
      }
    });
  }
}

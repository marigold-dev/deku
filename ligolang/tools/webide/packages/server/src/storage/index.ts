import { FileStorage } from './interface';
import { DiskStorage } from './disk-storage';

import process from 'process';
import { GoogleStorage } from './google-storage';

const isGoogleEnabled =
  process.env['GOOGLE_APPLICATION_CREDENTIALS'] && process.env['GOOGLE_BUCKET'];

function createStorage() {
  if (isGoogleEnabled) {
    return new GoogleStorage(process.env['GOOGLE_BUCKET']!);
  } else {
    return new DiskStorage(process.env['SHARED_FILE_STORAGE_PATH'] || '/tmp');
  }
}

export const storage: FileStorage = createStorage();

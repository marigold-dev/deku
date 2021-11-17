export abstract class CancellableAction {
  private cancelled = false;

  cancel() {
    this.cancelled = true;
  }

  isCancelled() {
    return this.cancelled;
  }

  abstract getAction(): any;
}

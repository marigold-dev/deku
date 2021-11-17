import joi from '@hapi/joi';

export abstract class Migration {
  protected abstract schema: joi.ObjectSchema;
  protected abstract previous: Migration | null;
  protected abstract migrate(data: any): any;

  validate(data: any): joi.ValidationResult {
    return this.schema.validate(data);
  }

  forward(data: any): any {
    const { error, value } = this.validate(data);

    if (error) {
      if (this.previous) {
        return this.migrate(this.previous.forward(data));
      }

      throw new Error(
        `Unable to migrate ${JSON.stringify(
          data
        )}. Reached the end of the migration chain.`
      );
    }
    return value;
  }
}

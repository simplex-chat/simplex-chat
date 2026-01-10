export enum MigrationConfirmation {
  YesUp = "yesUp",
  YesUpDown = "yesUpDown",
  Console = "console",
  Error = "error"
}

export interface CryptoArgs {
  fileKey: string
  fileNonce: string
}

export type DBMigrationError = 
  | DBMigrationError.InvalidConfirmation
  | DBMigrationError.ErrorNotADatabase // invalid/corrupt database file or incorrect encryption key
  | DBMigrationError.ErrorMigration
  | DBMigrationError.ErrorSQL

export namespace DBMigrationError {
  export type Tag = "invalidConfirmation" | "errorNotADatabase" | "errorMigration" | "errorSQL"

  interface Interface {
    type: Tag
  }

  export interface InvalidConfirmation extends Interface {
    type: "invalidConfirmation"
  }

  export interface ErrorNotADatabase extends Interface {
    type: "errorNotADatabase"
    dbFile: string
  }

  export interface ErrorMigration extends Interface {
    type: "errorMigration"
    dbFile: string
    migrationError: MigrationError
  }

  export interface ErrorSQL extends Interface {
    type: "errorSQL"
    dbFile: string
    migrationSQLError: string
  }
}

export type MigrationError =
  | MigrationError.MEUpgrade
  | MigrationError.MEDowngrade
  | MigrationError.MigrationError

export namespace MigrationError {
  export type Tag = "upgrade" | "downgrade" | "migrationError"

  interface Interface {
    type: Tag
  }

  export interface MEUpgrade extends Interface {
    type: "upgrade"
    upMigrations: UpMigration
  }

  export interface MEDowngrade extends Interface {
    type: "downgrade"
    downMigrations: [string]
  }

  export interface MigrationError extends Interface {
    type: "migrationError"
    mtrError: MTRError
  }  
}

export interface UpMigration {
  upName: string
  withDown: boolean
}

export type MTRError =
  | MTRError.MTRENoDown
  | MTRError.MTREDifferent

export namespace MTRError {
  export type Tag = "noDown" | "different"

  interface Interface {
    type: Tag
  }

  export interface MTRENoDown extends Interface {
    type: "noDown"
    upMigrations: UpMigration
  }

  export interface MTREDifferent extends Interface {
    type: "different"
    downMigrations: [string]
  }
}

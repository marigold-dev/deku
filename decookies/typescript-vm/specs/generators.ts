import * as fc from "fast-check";
import { createCookieBaker } from "../src/state";
// 77
export const cookieBakerArbitrary = () =>
  fc
    .tuple(
      fc.bigInt({ min: 0n, max: 2000000000n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n }),
      fc.bigInt({ min: 0n, max: 10n })
    )
    .map(
      ([
        cookies,
        cursors,
        grandmas,
        farms,
        mines,
        factories,
        banks,
        temples,
        wizards,
        shipments,
        alchemies,
        portals,
        timeMachines,
        antimatters,
        prisms,
        chanceMakers,
        fractals,
        javaScripts,
        idleverses,
        cordexs,
        eatenCookies,
      ]) => {
        const cookie_baker = createCookieBaker(
          cookies,
          cursors,
          grandmas,
          farms,
          mines,
          factories,
          banks,
          temples,
          wizards,
          shipments,
          alchemies,
          portals,
          timeMachines,
          antimatters,
          prisms,
          chanceMakers,
          fractals,
          javaScripts,
          idleverses,
          cordexs,
          eatenCookies
        );
        return cookie_baker;
      }
    );

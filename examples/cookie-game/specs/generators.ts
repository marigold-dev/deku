import * as fc from 'fast-check';
import { createCookieBaker } from '../src/state';
// 77
export const cookieBakerArbitrary = () =>
    fc.tuple(fc.bigInt({ min: 0n, max: 2000000000n }),
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
    ).map(
        ([cookies,
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
        ]) => {
            const freeCursor = BigInt(Math.floor(Math.random() * Number(cursors)));
            const freeGrandma = BigInt(Math.floor(Math.random() * Number(grandmas)));
            const freeFarm = BigInt(Math.floor(Math.random() * Number(farms)));
            const freeMine = BigInt(Math.floor(Math.random() * Number(mines)));
            const freeFactory = BigInt(Math.floor(Math.random() * Number(factories)));
            const freeBank = BigInt(Math.floor(Math.random() * Number(banks)));
            const freeTemple = BigInt(Math.floor(Math.random() * Number(temples)));
            const freeWizard = BigInt(Math.floor(Math.random() * Number(wizards)));
            const freeShipment = BigInt(Math.floor(Math.random() * Number(shipments)));
            const freeAlchemy = BigInt(Math.floor(Math.random() * Number(alchemies)));
            const freePortal = BigInt(Math.floor(Math.random() * Number(portals)));
            const freeTimeMachine = BigInt(Math.floor(Math.random() * Number(timeMachines)));
            const freeAntimatter = BigInt(Math.floor(Math.random() * Number(antimatters)));
            const freePrism = BigInt(Math.floor(Math.random() * Number(prisms)));
            const freeChanceMaker = BigInt(Math.floor(Math.random() * Number(chanceMakers)));
            const freeFractal = BigInt(Math.floor(Math.random() * Number(fractals)));
            const freeJavaScript = BigInt(Math.floor(Math.random() * Number(javaScripts)));
            const freeIdleverse = BigInt(Math.floor(Math.random() * Number(idleverses)));
            const freeCordex = BigInt(Math.floor(Math.random() * Number(cordexs)));
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
                cordexs
            )
            return cookie_baker;
        });

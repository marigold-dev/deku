/**
 * To wait for an operation to be included
 * TODO: this function have to be include in the deku toolkit
 * @param dekuRpc the url of deku
 * @param operation the hash of the operation (the one starting by "Do")
 * @param tries the number of try you want to make to check the inclusion of your operation
 * @return the operation hash
 */
export const wait = async (dekuRpc:string, operation:string, tries=10) : Promise<string> => {
    const aux = (tries:number): Promise<string> => {
      return fetch(`${dekuRpc}/api/v1/operations/${operation}`)
        .then(res => {
            if(res.ok) return operation;
            throw "Try again"
        })
          .catch(() => {
            if(tries < 0) throw `Operation ${operation} has not been applied`
            return new Promise(resolve => setTimeout(() => aux(tries - 1).then(resolve), 1000))
        })
    };
    return aux(tries);
}
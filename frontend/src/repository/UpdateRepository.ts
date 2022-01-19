export default class UpdateRepository {
    private static readonly API_URL: string = `${process.env.REACT_APP_API_URL || ""}update`;

    public async getLastUpdate(): Promise<any[]> {
        const options: RequestInit = {
            method: "GET",
            headers: { "Content-Type": "application/json" }
        }

        const result: any[] = (await (await fetch(`${UpdateRepository.API_URL}?limit=1&offset=0`, options)).json()).updates

        return result;
    }

    public async update() : Promise<any[]> {
        const options: RequestInit = {
            method: "POST",
            headers: { "Content-Type": "application/json" },
        }

        const result: any[] = (await (await fetch(`${UpdateRepository.API_URL}`, options)).json())

        return result;
    }
}

import Pagination from "./../model/Pagination/Pagination";
import Filter from "./../model/Filter/Filter";

export default class DateRepository {
    private static readonly API_URL: string = `${process.env.REACT_APP_API_URL || ""}date/dose`;

    public async list(pagination: Pagination, filter: Filter): Promise<any[]> {
        const options: RequestInit = {
            method: "GET",
            headers: { "Content-Type": "application/json" }
        }

        const result: any[] = (await (await fetch(`${DateRepository.API_URL}?limit=${pagination.limit}&offset=${pagination.offset}`, options)).json()).dates

        return result.sort((b,a) => b.date.localeCompare(a.date));
    }
}

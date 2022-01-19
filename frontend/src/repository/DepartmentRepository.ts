import Pagination from "./../model/Pagination/Pagination";
import Filter from "./../model/Filter/Filter";

export default class DepartmentRepository {
    private static readonly API_URL: string = `${process.env.REACT_APP_API_URL || ""}department`;

    public async list(pagination: Pagination, filter: Filter): Promise<any[]> {
        const options: RequestInit = {
            method: "GET",
            headers: { "Content-Type": "application/json" }
        }

        const result: any[] = (await (await fetch(`${DepartmentRepository.API_URL}?limit=${pagination.limit}&offset=${pagination.offset}`, options)).json()).departments

        return result;
    }

    public async post(name: string) : Promise<any[]> {
        const options: RequestInit = {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: `{ "department": { "name": "${name}" } }`
        }

        const result: any[] = (await (await fetch(`${DepartmentRepository.API_URL}`, options)).json())

        return result;
    }

    public async delete(id: number) : Promise<any> {
        const options: RequestInit = {
            method: "DELETE",
            headers: { "Content-Type": "application/json" },
        }

        const result: any = (await fetch(`${DepartmentRepository.API_URL}/${id}`, options))

        return result;
    }

    public async deleteAll() : Promise<any> {
        const options: RequestInit = {
            method: "DELETE",
            headers: { "Content-Type": "application/json" },
        }

        const result: any = (await fetch(`${DepartmentRepository.API_URL}`, options))

        return result;
    }
}

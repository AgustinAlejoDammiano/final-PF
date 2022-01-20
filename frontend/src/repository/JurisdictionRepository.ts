import Pagination from "./../model/Pagination/Pagination";
import Filter from "./../model/Filter/Filter";

export default class JurisdictionRepository {
    private static readonly API_URL: string = `${process.env.REACT_APP_API_URL || ""}jurisdiction`;

    public async list(pagination: Pagination, filter: Filter): Promise<any[]> {
        const options: RequestInit = {
            method: "GET",
            headers: { "Content-Type": "application/json" }
        }

        const result: any[] = (await (await fetch(`${JurisdictionRepository.API_URL}?limit=${pagination.limit}&offset=${pagination.offset}`, options)).json()).jurisdictions

        return result;
    }


    public async listDose(pagination: Pagination, filter: Filter): Promise<any[]> {
        const options: RequestInit = {
            method: "GET",
            headers: { "Content-Type": "application/json" }
        }

        const result: any[] = (await (await fetch(`${JurisdictionRepository.API_URL}/dose?limit=${pagination.limit}&offset=${pagination.offset}`, options)).json()).jurisdictions

        return result.sort((a, b) => b.totalDose - a.totalDose);
    }

    public async post(name: string) : Promise<any[]> {
        const options: RequestInit = {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: `{ "jurisdiction": { "name": "${name}" } }`
        }

        const result: any[] = (await (await fetch(`${JurisdictionRepository.API_URL}`, options)).json())

        return result;
    }

    public async delete(id: number) : Promise<any> {
        const options: RequestInit = {
            method: "DELETE",
            headers: { "Content-Type": "application/json" },
        }

        const result: any = (await fetch(`${JurisdictionRepository.API_URL}/${id}`, options))

        return result;
    }

    public async deleteAll() : Promise<any> {
        const options: RequestInit = {
            method: "DELETE",
            headers: { "Content-Type": "application/json" },
        }

        const result: any = (await fetch(`${JurisdictionRepository.API_URL}`, options))

        return result;
    }
}

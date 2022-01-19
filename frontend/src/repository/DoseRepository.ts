import Pagination from "./../model/Pagination/Pagination";
import Filter from "./../model/Filter/Filter";
import { format } from "date-fns";

export default class DoseRepository {
    private static readonly API_URL: string = `${process.env.REACT_APP_API_URL || ""}dose`;

    public async list(pagination: Pagination, filter: Filter): Promise<any[]> {
        const options: RequestInit = {
            method: "GET",
            headers: { "Content-Type": "application/json" }
        }

        const result: any[] = (await (await fetch(`${DoseRepository.API_URL}?limit=${pagination.limit}&offset=${pagination.offset}`, options)).json()).doses

        return result;
    }

    public async post(sex: string, age: string, condition: string, lot: string, serie: string, vaccineId: string, 
        residenceJurisdictionId: string, residenceDepartmentId: string, applicationJurisdictionId: string, 
        applicationDepartmentId: string, date: Date) : Promise<any[]> {
        const options: RequestInit = {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: `{ 
                "dose": { 
                    "sex": "${sex}", 
                    "age": "${age}", 
                    "condition": "${condition}", 
                    "lot" : "${lot}", 
                    "date" : "${format(date, "yyy-MM-dd")}", 
                    "serie" : "${serie}", 
                    "vaccineId" : "${vaccineId}",  
                    "residenceJurisdictionId" : "${residenceJurisdictionId}", 
                    "residenceDepartmentId" : "${residenceDepartmentId}", 
                    "applicationJurisdictionId" : "${applicationJurisdictionId}", 
                    "applicationDepartmentId" : "${applicationDepartmentId}" 
                } 
            }`
        }

        const result: any[] = (await (await fetch(`${DoseRepository.API_URL}`, options)).json())

        return result;
    }

    public async delete(id: number) : Promise<any> {
        const options: RequestInit = {
            method: "DELETE",
            headers: { "Content-Type": "application/json" },
        }

        const result: any = (await fetch(`${DoseRepository.API_URL}/${id}`, options))

        return result;
    }

    public async deleteAll() : Promise<any> {
        const options: RequestInit = {
            method: "DELETE",
            headers: { "Content-Type": "application/json" },
        }

        const result: any = (await fetch(`${DoseRepository.API_URL}`, options))

        return result;
    }
}

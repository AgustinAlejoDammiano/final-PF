import React, { useState, useEffect } from "react";
import useStyles from "./styles";
import { Button, TextField, Typography, FormControl, InputLabel, Select } from "@material-ui/core";
import CircularProgress from '@material-ui/core/CircularProgress';
import DoseRepository from "../../repository/DoseRepository";
import VaccineRepository from "../../repository/VaccineRepository";
import Pagination from "../../model/Pagination/Pagination";
import Filter from "../../model/Filter/Filter";
import JurisdictionRepository from "../../repository/JurisdictionRepository";
import DepartmentRepository from "../../repository/DepartmentRepository";
import DatePicker from "react-datepicker";

import "react-datepicker/dist/react-datepicker.css";

const repository = new DoseRepository()
const repositoryVaccine = new VaccineRepository()
const repositoryJurisdiction = new JurisdictionRepository()
const repositoryDepartment = new DepartmentRepository()

export default function DoseFrom() {
    const classes = useStyles();

    const [sex, setSex] = useState<string | undefined>(undefined);
    const [age, setAge] = useState<string | undefined>(undefined);
    const [condition, setCondition] = useState<string | undefined>(undefined);
    const [lot, setLot] = useState<string | undefined>(undefined);
    const [serie, setSerie] = useState<string | undefined>(undefined);
    const [vaccineId, setVaccineId] = useState<string | undefined>(undefined);
    const [residenceJurisdictionId, setResidenceJurisdictionId] = useState<string | undefined>(undefined);
    const [residenceDepartmentId, setResidenceDepartmentId] = useState<string | undefined>(undefined);
    const [applicationJurisdictionId, setApplicationJurisdictionId] = useState<string | undefined>(undefined);
    const [applicationDepartmentId, setApplicationDepartmentId] = useState<string | undefined>(undefined);
    const [date, setDate] = useState(new Date());
    const [loading, setLoading] = React.useState(false);

    const onSummit = () => {
        if (condition !== undefined && sex !== undefined && age !== undefined && lot !== undefined
            && serie !== undefined && vaccineId !== undefined && residenceJurisdictionId !== undefined 
            && residenceDepartmentId !== undefined && applicationJurisdictionId !== undefined 
            && applicationDepartmentId !== undefined) {
            setLoading(true)
            repository.post(sex, age, condition, lot, serie, vaccineId, 
                residenceJurisdictionId, residenceDepartmentId, applicationJurisdictionId, 
                applicationDepartmentId, date)
                .then((r) => window.location.reload()) //TODO maybe add toast
                .catch((error: Error) => console.log(error))
                .finally(() => setLoading(false))
        }
    }

    const getOptionVaccines = () => {
        return repositoryVaccine.list(new Pagination(10000, 0), new Filter())
    }

    const getOptionJurisdiction = () => {
        return repositoryJurisdiction.list(new Pagination(10000, 0), new Filter())
    }

    const getOptionDepartment = () => {
        return repositoryDepartment.list(new Pagination(10000, 0), new Filter())
    }

    return (
        <div className={classes.rowContainer}>
            <div className={classes.rowContainer}>
                <FormControl variant="outlined" className={classes.formControl}>
                    <InputLabel>{"Sex"}</InputLabel>
                    <Select native value={sex} label={"Sex"}
                        onChange={(e) => setSex(e.target.value as string)} >
                        <option aria-label="None" value={undefined} />
                        <option value={"F"}>{"Female"}</option>
                        <option value={"M"}>{"Male"}</option>
                    </Select>
                </FormControl>
            </div>
            <div className={classes.rowContainer}>
                <FormControl variant="outlined" className={classes.formControl}>
                    <InputLabel>{"Age"}</InputLabel>
                    <Select native value={age} label={"Age"}
                        onChange={(e) => setAge(e.target.value as string)} >
                        <option aria-label="None" value={undefined} />
                        <option value={"<12"}>{"<12"}</option>
                        <option value={"12-17"}>{"12-17"}</option>
                        <option value={"18-29"}>{"18-29"}</option>
                        <option value={"30-39"}>{"30-39"}</option>
                        <option value={"40-49"}>{"40-49"}</option>
                        <option value={"50-59"}>{"50-59"}</option>
                        <option value={"60-69"}>{"60-69"}</option>
                        <option value={"70-79"}>{"70-79"}</option>
                        <option value={"80-89"}>{"80-89"}</option>
                        <option value={"90-99"}>{"90-99"}</option>
                        <option value={">=100"}>{">=100"}</option>
                    </Select>
                </FormControl>
            </div>
            <div className={classes.rowContainer} style={{paddingLeft: "2rem", flexDirection: "row", justifyContent: "Left"}}>
                <Typography variant="h6" style={{paddingRight: "1rem"}}>Date:</Typography>
                <DatePicker selected={date} onChange={(d: any) => setDate(d)} />
            </div>
            <div className={classes.rowContainer}>
                <TextField className={classes.formControl} label={"Condition"}
                    multiline rows={1} variant="outlined" onChange={(e) => setCondition(e.target.value as string)} />
            </div>
            <div className={classes.rowContainer}>
                <TextField className={classes.formControl} label={"Lot"}
                    multiline rows={1} variant="outlined" onChange={(e) => setLot(e.target.value as string)} />
            </div>
            <div className={classes.rowContainer}>
                <FormControl variant="outlined" className={classes.formControl}>
                    <InputLabel>{"Serie"}</InputLabel>
                    <Select native value={serie} label={"Serie"}
                        onChange={(e) => setSerie(e.target.value as string)} >
                        <option aria-label="None" value={undefined} />
                        <option value={"1"}>{"First Dose"}</option>
                        <option value={"2"}>{"Second dose"}</option>
                        <option value={"3"}>{"Third dose"}</option>
                    </Select>
                </FormControl>
            </div>
            <CustomOptions onChange={setVaccineId} value={vaccineId} title={"Vaccine"} getOptions={getOptionVaccines} />
            <CustomOptions onChange={setResidenceJurisdictionId} value={residenceJurisdictionId}
                title={"Residence jurisdiction"} getOptions={getOptionJurisdiction} />
            <CustomOptions onChange={setResidenceDepartmentId} value={residenceDepartmentId}
                title={"Residence department"} getOptions={getOptionDepartment} />
            <CustomOptions onChange={setApplicationJurisdictionId} value={applicationJurisdictionId}
                title={"Application jurisdiction"} getOptions={getOptionJurisdiction} />
            <CustomOptions onChange={setApplicationDepartmentId} value={applicationDepartmentId}
                title={"Application department"} getOptions={getOptionDepartment} />
            <div className={classes.rowContainer}>
                <Button className={classes.button} onClick={onSummit} disabled={loading || condition === undefined
                    || sex === undefined || age === undefined || lot === undefined || serie === undefined
                    || vaccineId === undefined || residenceJurisdictionId === undefined || residenceDepartmentId === undefined
                    || applicationJurisdictionId === undefined || applicationDepartmentId === undefined}>
                    <Typography variant="h6">Create</Typography>
                </Button>
                {loading && <CircularProgress size={24} className={classes.buttonProgress} />}
            </div>
        </div >
    )
}

type Props = {
    onChange: (value: string) => void,
    value: string | undefined,
    title: string,
    getOptions: () => Promise<any[]>
}

function CustomOptions({ onChange, value, title, getOptions }: Props) {

    const classes = useStyles();

    const [options, setOptions] = useState<any[]>([]);

    useEffect(() => {
        getOptions()
            .then((r) => setOptions(r))
            .catch((error) => console.log(error)) //TODO
    }, [getOptions])

    return (
        <div className={classes.rowContainer}>
            <FormControl variant="outlined" className={classes.formControl}>
                <InputLabel>{title}</InputLabel>
                <Select native value={value} label={title}
                    onChange={(e) => onChange(e.target.value as string)} >
                    <option aria-label="None" value={undefined} />
                    {
                        options.map((v) => <option value={v.id}>{v.name}</option>)
                    }
                </Select>
            </FormControl>
        </div>
    )
}

import React, { useState } from "react";
import useStyles from "./styles";
import Pagination from "./../../model/Pagination/Pagination";
import Filter from "./../../model/Filter/Filter";
import { Grid, Button, Typography } from '@material-ui/core';
import DeleteIcon from '@material-ui/icons/Delete';
import DoseRepository from "../../repository/DoseRepository";
import DoseFrom from "../DoseFrom/DoseFrom";
import withList from "../List/withList";

const repository = new DoseRepository()

export default function DoseList() {

    const [deleteDoses, setDeleteDoses] = useState<any[]>([])

    const getPage = async (handleNewList: ((doseList: any[]) => void),
        pagination: Pagination, filter: Filter) => {
        repository.list(pagination, filter)
            .then((doseListResponce) => {
                handleNewList(doseListResponce);
            })
            .catch((error) => console.log(error)) //TODO
    }

    const deleteDose = (dose: any) => {
        return () => {
            repository.delete(dose.id)
                .then(() => {
                    setDeleteDoses((l) => [...l, dose])
                })
                .catch((error) => console.log(error)) //TODO
        }
    }

    const onDelete = () => {
        repository.deleteAll()
                .then(() => window.location.reload())
                .catch((error) => console.log(error)) //TODO
    }

    const classes = useStyles();

    const renderDose = (dose: any) => {
        if (deleteDoses.includes(dose)) {
            return null
        }
        return (
            <Grid container className={classes.gridContainer} spacing={3}>
                <Grid item className={classes.item}><Typography variant="body2">{dose.id}</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="body2">{dose.sex === "F" ? "Female" : "Male"}</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="body2">{dose.age}</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="body2">{dose.age}</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="body2">{dose.lot}</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="body2">{dose.date}</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="body2">{dose.serie}</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="body2">{dose.vaccineId}</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="body2">{dose.residenceJurisdictionId}</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="body2">{dose.residenceDepartmentId}</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="body2">{dose.applicationJurisdictionId}</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="body2">{dose.applicationDepartmentId}</Typography></Grid>
                <Grid item className={classes.item}>
                    <Button size="small" color="primary" onClick={deleteDose(dose)}>
                        <DeleteIcon />
                    </Button>
                </Grid>
            </Grid>
        )
    }

    const renderTitles = () => {
        return (
            <Grid container className={classes.gridContainer} spacing={3}>
                <Grid item className={classes.item}><Typography variant="h5">ID</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="h5">Sex</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="h5">Age</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="h5">Condition</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="h5">Lot</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="h5">Date</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="h5">Serie</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="h5">Vaccine ID</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="h5">Residence Juristiction ID</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="h5">Residence Department ID</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="h5">Application Juristiction ID</Typography></Grid>
                <Grid item className={classes.item}><Typography variant="h5">Application Department ID</Typography></Grid>
                <Grid item className={classes.item}>
                    <Button size="small" color="primary" disabled></Button>
                </Grid>
            </Grid>
        )
    }

    const title = "Dose Form"

    const DoseList = withList(DoseFrom, getPage, renderTitles, renderDose, title, onDelete)

    return <DoseList />
}

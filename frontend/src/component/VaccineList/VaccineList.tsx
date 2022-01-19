import React, { useState } from "react";
import useStyles from "./styles";
import Pagination from "./../../model/Pagination/Pagination";
import Filter from "./../../model/Filter/Filter";
import { Grid, Button, Typography } from '@material-ui/core'
import DeleteIcon from '@material-ui/icons/Delete';
import VaccineRepository from "../../repository/VaccineRepository";
import VaccineFrom from "../VaccineFrom/VaccineFrom";
import withList from "../List/withList";

const repository = new VaccineRepository()

export default function VaccineList() {

    const [deleteVaccines, setDeleteVaccines] = useState<any[]>([])

    const getPage = async (handleNewList: ((vaccineList: any[]) => void),
        pagination: Pagination, filter: Filter) => {
        repository.list(pagination, filter)
            .then((vaccineListResponce) => {
                handleNewList(vaccineListResponce);
            })
            .catch((error) => console.log(error)) //TODO
    }

    const deleteVaccine = (vaccine: any) => {
        return () => {
            repository.delete(vaccine.id)
                .then(() => {
                    setDeleteVaccines((l) => [...l, vaccine])
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

    const renderVaccine = (vaccine: any) => {
        if (deleteVaccines.includes(vaccine)) {
            return null
        }
        return (
            <Grid container className={classes.gridContainer} spacing={3}>
                <Grid item><Typography variant="body2">{vaccine.id}</Typography></Grid>
                <Grid item><Typography variant="body2">{vaccine.name}</Typography></Grid>
                <Grid item>
                    <Button size="small" color="primary" onClick={deleteVaccine(vaccine)}>
                        <DeleteIcon />
                    </Button>
                </Grid>
            </Grid>
        )
    }

    const renderTitles = () => {
        return (
            <Grid container className={classes.gridContainer} spacing={3}>
                <Grid item><Typography variant="h5">ID</Typography></Grid>
                <Grid item><Typography variant="h5">Name</Typography></Grid>
                <Grid item>
                    <Button size="small" color="primary" disabled></Button>
                </Grid>
            </Grid>
        )
    }

    const title = "Vaccine Form"

    const VaccineList = withList(VaccineFrom, getPage, renderTitles, renderVaccine, title, onDelete)

    return <VaccineList />
}

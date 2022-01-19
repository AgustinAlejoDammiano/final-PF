import React, { useState } from "react";
import useStyles from "./styles";
import Pagination from "./../../model/Pagination/Pagination";
import Filter from "./../../model/Filter/Filter";
import { Grid, Button, Typography } from '@material-ui/core'
import DeleteIcon from '@material-ui/icons/Delete';
import DepartmentRepository from "../../repository/DepartmentRepository";
import DepartmentFrom from "../DepartmentFrom/DepartmentFrom";
import withList from "../List/withList";

const repository = new DepartmentRepository()

export default function DepartmentList() {

    const [deleteDepartments, setDeleteDepartments] = useState<any[]>([])

    const getPage = async (handleNewList: ((departmentList: any[]) => void),
        pagination: Pagination, filter: Filter) => {
        repository.list(pagination, filter)
            .then((departmentListResponce) => {
                handleNewList(departmentListResponce);
            })
            .catch((error) => console.log(error)) //TODO
    }

    const deleteDepartment = (department: any) => {
        return () => {
            repository.delete(department.id)
                .then(() => {
                    setDeleteDepartments((l) => [...l, department])
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

    const renderDepartment = (department: any) => {
        if (deleteDepartments.includes(department)) {
            return null
        }
        return (
            <Grid container className={classes.gridContainer} spacing={3}>
                <Grid item><Typography variant="body2">{department.id}</Typography></Grid>
                <Grid item><Typography variant="body2">{department.name}</Typography></Grid>
                <Grid item>
                    <Button size="small" color="primary" onClick={deleteDepartment(department)}>
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

    const title = "Department Form"

    const DepartmentList = withList(DepartmentFrom, getPage, renderTitles, renderDepartment, title, onDelete)

    return <DepartmentList />
}

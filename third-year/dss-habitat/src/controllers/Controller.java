/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package controllers;

import data.DataException;
import java.util.List;
import java.util.Map;

/**
 *
 * @author mendes
 */
public interface Controller<T> {
    public T save(Map<String, Object> params) throws DataException;
    public void save(T t) throws DataException;
    public void updateAll(List<T> t) throws DataException;
    public List<T> saveAll(List<Map<String, Object>> params) throws DataException;
    public T find(int id) throws DataException;
    public T newInstance(Map<String, Object> params) throws DataException;
    public List<T> findBy(Map<String, Object> params) throws DataException;
    public List<T> all() throws DataException;
    public void delete(T t) throws DataException;
}
